-- |
-- Module      : Spicy.Molecule.Internal.Multipoles
-- Description : Operations and transformations of multipole data
-- Copyright   : Phillip Seeber, 2020
-- License     : GPL-3
-- Maintainer  : phillip.seeber@uni-jena.de
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This module provides operations for multipole data, including transformation to local frames and
-- conversion to point charges. For most of the definitions and conversion used see
-- [A Novel, Computationally Efficient Multipolar Model Employing Distributed Charges for Molecular Dynamics Simulations](https://pubs.acs.org/doi/10.1021/ct500511t).
--
-- The multipole moments up to the quadrupoles are defined as spherical tensors. Those multipoles
-- can be exactly represented as an octahedral point charge model in the axis system of the
-- quadrupole tensor, see 'toOctrahedralModel'.
--
-- This octahedral model defined in the coordinate system of the quadrupole tensor must be rotated
-- to the actual orientation of the molecule. For this purpose a local, orthogonal axis system is
-- defined. The original literature defines only for completely bonded systems of at least 3
-- non-colinear atoms how to define the axes system. In our case we do not treat completely bonded
-- molecular systems, but whatever is given in a 'fragment' field of a 'Molecule', which potentially
-- involves non-bonded atoms, only colinear atoms and other nasty cases that complicate finding a
-- triple of three non-bonded atoms. Those cases are dealt with in 'selectCases'. To be able to
-- treat corner cases correctly we also allow definition of groups of no or one patner atoms only,
-- if no other atom is close by to define a proper reference system. In the case of no bond partner
-- available the octahedron will not be rotated and the charges will be on the cartesian axes of the
-- atom centred cartesian coordinate system. In the case of two binding partners only the z-axis
-- will be defined and x and y will be chosen arbitrarily. This is legitimate if no other
-- interacting partners are close and the charge distribution is spherical (no partner) or
-- rotational symmetric (one partner).
module Spicy.Molecule.Internal.Multipoles
  ( OctahedralModel (..),
    molToPointCharges,
    toOctrahedralModel,
    sphericalToLocal,
    BestBondPartners (..),
    makeReferenceGroups,
    groupsInFrag,
    selectCases,
    makeLocalAxesSystemE,
  )
where

import qualified Data.IntMap as IntMap
import Data.Massiv.Array as Massiv hiding (swap)
import Data.Tuple (swap)
import Optics hiding (Empty, (<|), (|>))
import RIO hiding (Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.Map as Map
import RIO.Seq ((|>))
import qualified RIO.Seq as Seq
import Spicy.Common
import Spicy.Data
import Spicy.Math
import Spicy.Molecule.Internal.Types hiding (S, coordinates)
import Spicy.Molecule.Internal.Util

-- | The minimum distance a neighbourlist needs to find bond partners for reference frame
-- definition.
cutoffDistance :: Double
cutoffDistance = 10

----------------------------------------------------------------------------------------------------

-- | The multipoles can be converted to an octahedral point charge model. Those charges might be
-- defined with respect to the principal axis of the spherical quadrupole tensor or in a local
-- reference frame of the molecule.
data OctahedralModel
  = SphericalRef
      { -- | The local axes system (3 x 3 matrix of basis vectors).
        axesSystem :: !(Matrix S Double),
        -- | The point charges as a 4 x 6 matrix. The columns represent one point charge each.
        -- The first three rows are cartesian coordinates in angstrom, the fourth row is the
        -- magnitude of the point charge.
        values :: !(Matrix S Double)
      }
  | LocalRef
      { -- | The local axes system (3 x 3 matrix of basis vectors).
        axesSystem :: !(Matrix S Double),
        -- | The point charges as a 4 x 6 matrix. The columns represent one point charge each.
        -- The first three rows are cartesian coordinates in angstrom, the fourth row is the
        -- magnitude of the point charge.
        values :: !(Matrix S Double)
      }

-- Lenses
instance (k ~ A_Lens, a ~ Matrix S Double, b ~ a) => LabelOptic "axesSystem" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> axesSystem s) $ \s b -> s {axesSystem = b}

instance (k ~ A_Lens, a ~ Matrix S Double, b ~ a) => LabelOptic "values" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> values s) $ \s b -> s {values = b}

isSphericalRef :: OctahedralModel -> Bool
isSphericalRef ref = case ref of
  SphericalRef {} -> True
  _ -> False

----------------------------------------------------------------------------------------------------

-- | Converts the entire current layer of the molecule to a point charge representation, where to
-- multipole moments up to quadrupoles are represented as point charges. Only dummy atoms will
-- have a multipole point charge model in the result, but the other atoms might be used for bonding
-- information. The point charges will be represented in a \(4 \times (p N)\) matrix, where \(p\) is
-- the number of point charges used per atom to expand the multipole moments, and \(N\) is the
-- number of dummy atoms in the molecule. See also 'OctahedralModel'.
molToPointCharges :: (MonadThrow m, MonadIO m) => Molecule -> m (Matrix S Double)
molToPointCharges mol = do
  let -- Obtain some initial information.
      atoms = mol ^. #atoms
      dummyAtoms = IntMap.keysSet . IntMap.filter (\a -> a ^. #isDummy) $ atoms

      -- Make an octahedral point charge model per dummy atom.
      dummiesWithSph =
        fmap (\a -> toOctrahedralModel Nothing $ a ^. #multipoles)
          . IntMap.filter (\a -> a ^. #isDummy)
          $ atoms

  -- Obtain the groups of atoms in which the reference frames will be defined.
  localRefGroups <- makeReferenceGroups mol
  localAxesSystems <-
    IntMap.unions
      <$> traverse (\x -> flip IntMap.restrictKeys dummyAtoms <$> makeLocalAxesSystemE x) localRefGroups

  -- Transform all spherical models to local models in their respective reference frames.
  dummiesLocalModel <-
    sequence $
      IntMap.intersectionWith
        ( \dummySpherical localAxesSystem ->
            sphericalToLocal localAxesSystem dummySpherical
        )
        dummiesWithSph
        localAxesSystems

  -- Generate the 4x(6N) matrix of the octahedral charge models. N is the number dummy atoms.
  -- Has one column per point charge, the first three rows being the cartesian coordinates of the
  coordChargeMatPerAtom <-
    sequence $
      IntMap.intersectionWith
        ( \atom localModel -> do
            let valueMat = delay $ localModel ^. #values
                Sz (m :. n) = size valueMat
                atomCoords = expandInner (Sz n) const . getVectorS $ atom ^. #coordinates

            unless (m == 4) . throwM . localExc $
              "Representation of the point charges requires a matrix with 4 rows."

            relativeChargeCoords <- extractFromToM (0 :. 0) (2 :. n - 1) valueMat
            computeS @S <$> atomCoords .+. relativeChargeCoords
        )
        atoms
        dummiesLocalModel

  coordChargeMat <- computeP <$> concatM 1 coordChargeMatPerAtom

  return coordChargeMat
  where
    localExc = MolLogicException "molToPointCharges"

----------------------------------------------------------------------------------------------------

-- | Converts the atomic multipoles to the octahedral charge model in the coordinate system of the
-- spherical quadrupole tensor. All moments higher than quadrupole will be neglected. The octahedral
-- charge model follows as the solution of a linear equation system with the following solutions:
-- \[ q_{(d_\mathrm{q}, 0, 0)}  = \frac{Q_{00}}{6}                + \frac{Q_{11c}}{2 d_\mathrm{q}}
--                              - \frac{Q_{20}}{6 d_\mathrm{q}^2} + \frac{Q_{22c}}{2 \sqrt{3} d_\mathrm{q}^2}
-- \]
-- \[ q_{(0, -d_\mathrm{q}, 0)} = \frac{Q_{00}}{6}                - \frac{Q_{11s}}{2 d_\mathrm{q}}
--                              - \frac{Q_{20}}{6 d_\mathrm{q}^2} + \frac{Q_{22c}}{2 \sqrt{3} d_\mathrm{q}^2}
-- \]
-- \[ q_{(-d_\mathrm{q}, 0, 0)} = \frac{Q_{00}}{6}                - \frac{Q_{11c}}{2 d_\mathrm{q}}
--                              - \frac{Q_{20}}{6 d_\mathrm{q}^2} + \frac{Q_{22c}}{2 \sqrt{3} d_\mathrm{q}^2}
-- \]
-- \[ q_{(0, 0, d_\mathrm{q})}  = \frac{Q_{00}}{6}                - \frac{Q_{10}}{2 d_\mathrm{q}}
--                              + \frac{Q_{20}}{3 d_\mathrm{q}^2}
-- \]
-- \[ q_{(0, d_\mathrm{q}, 0)}  = \frac{Q_{00}}{6}                + \frac{Q_{11s}}{2 d_\mathrm{q}}
--                              - \frac{Q_{20}}{6 d_\mathrm{q}^2} - \frac{Q_{22c}}{2 \sqrt{3} d_\mathrm{q}^2}
-- \]
-- \[ q_{(0, 0, -d_\mathrm{q})} = \frac{Q_{00}}{6}                - \frac{Q_{10}}{2 d_\mathrm{q}}
--                              + \frac{Q_{20}}{3 d_\mathrm{q}^2}
-- \]
toOctrahedralModel ::
  -- | The distance from the origin \(d_\mathrm{q}\) in Angstrom. Default to \( \frac{1}{4} a_0 \)
  Maybe Double ->
  -- | The multipoles to convert.
  Multipoles ->
  -- | The octahedral point charge representation of the multipoles in the principla axis of the
  -- spherical quadrupole tensor.
  OctahedralModel
toOctrahedralModel dist mp =
  let -- Input values expanded into components.
      d = fromMaybe (bohr2Angstrom 0.25) (bohr2Angstrom <$> dist)
      q00 = fromMaybe 0 $ mp ^? #monopole % _Just % #q00
      q10 = fromMaybe 0 $ mp ^? #dipole % _Just % #q10
      q11c = fromMaybe 0 $ mp ^? #dipole % _Just % #q11c
      q11s = fromMaybe 0 $ mp ^? #dipole % _Just % #q11s
      q20 = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q20
      q22c = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q22c
      e2 = 2 :: Int

      -- The values of the point charges.
      qXu = (q00 / 6) + (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2))
      qYd = (q00 / 6) - (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2))
      qXd = (q00 / 6) - (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2))
      qZu = (q00 / 6) + (q10 / (2 * d)) + (q20 / (3 * d ^ e2))
      qYu = (q00 / 6) + (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2))
      qZd = (q00 / 6) - (q10 / (2 * d)) + (q20 / (3 * d ^ e2))

      -- Combine them to the matrix representation.
      chargeMagnitudes = Massiv.fromLists' @S @Ix2 @Double Seq [[qXu, qXd, qYu, qYd, qZu, qZd]]
      relativeCoordinates =
        Massiv.fromLists' @S @Ix2 @Double
          Seq
          --  Xu,  Xd,  Yu,  Yd, Zu, Zd
          [ [d, - d, 0, 0, 0, 0],
            [0, 0, d, - d, 0, 0],
            [0, 0, 0, 0, d, - d]
          ]
      valueMat = computeS $ append' 2 relativeCoordinates chargeMagnitudes
   in SphericalRef
        { axesSystem = computeS . identityMatrix $ Sz 3,
          values = valueMat
        }

----------------------------------------------------------------------------------------------------

-- | Conversion of a vector from the spherical axis system to the local axis system.
--
-- The coordinate transfomation of a charge \(q\) with coordinates \( u \hat{\mathbf{p}}_x \),
-- \( v \hat{\mathbf{p}}_y \) and \( w \hat{\mathbf{p}}_z \), where \( \hat{\mathbf{p}}_x \),
-- \( \hat{\mathbf{p}}_y \) and \( \hat{\mathbf{p}}_z \) are the principal axes of the quadrupole
-- tensor, to the coordinates \( u' \hat{\mathbf{e}}_x \),- \( v' \hat{\mathbf{e}}_y \) and
-- \( w' \hat{\mathbf{e}}_z \) in the local atomic frame happens by the following equations:
-- \[ u' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_x + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_x + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_x \]
-- \[ v' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_y + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_y + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_x \]
-- \[ w' = u \hat{\mathbf{p}}_x \cdot \hat{\mathbf{e}}_z + v \hat{\mathbf{p}}_y \cdot \hat{\mathbf{e}}_z + w \hat{\mathbf{p}}_z \cdot \hat{\mathbf{e}}_z \]
-- With the local axes systems in \(3 \times 3 \) matrix form
-- \[ \mathbf{P} = \begin{bmatrix} \hat{\mathbf{p}}_x & \hat{\mathbf{p}}_y & \hat{\mathbf{p}}_z \end{bmatrix} \]
-- \[ \mathbf{E} = \begin{bmatrix} \hat{\mathbf{e}}_x & \hat{\mathbf{e}}_y & \hat{\mathbf{e}}_z \end{bmatrix} \]
-- and the coordinates of the point charges with column vectors of a matrix:
-- \[ \mathbf{C} = \begin{bmatrix}
--      d_\mathrm{q} & -d_\mathrm{q} &            0 &             0 &            0 &             0 \\
--                 0 &             0 & d_\mathrm{q} & -d_\mathrm{q} &            0 &             0 \\
--                 0 &             0 &            0 &             0 & d_\mathrm{q} & -d_\mathrm{q}
--    \end{bmatrix}
-- \]
-- the expression can be implemented with linear algebra:
-- \[ \mathbf{C}' = \mathbf{P}^T \mathbf{E} \mathbf{C} \]
sphericalToLocal ::
  (MonadThrow m) =>
  -- | The matrix \(\mathbf{E}\)
  Matrix S Double ->
  -- | Spherical representation of the octahedral charge model with the matrix \(\mathbf{P}\).
  OctahedralModel ->
  -- | The octahedral model rotated into the local reference frame with the matrix \(\mathbf{C}'\)
  -- as part of the result.
  m OctahedralModel
sphericalToLocal matE sphModel = do
  -- Perform some preliminary checks.
  let valueMat = sphModel ^. #values
      Sz (mC :. nC) = size valueMat
  unless (isSphericalRef sphModel) . throwM . localExc $
    "Input reference system is not spherical."
  unless (mC == 4) . throwM . localExc $ "Value matrix of point charges has wrong number of rows."

  let matP = sphModel ^. #axesSystem
      matPT = computeAs S . setComp Seq . transpose $ matP
  matC <- extractFromToM (0 :. 0) (2 :. nC - 1) valueMat
  chargeMag <- extractFromToM (3 :. 0) (3 :. nC - 1) valueMat

  -- The matrix equation applied.
  matC' <- matPT .><. matE >>= \pe -> (computeS @S pe) .><. matC

  -- Reconstruction of the value matrix with the magnitudes of charges readded.
  valueMat' <- computeS @S <$> appendM 2 matC' chargeMag

  return $
    LocalRef
      { values = valueMat',
        axesSystem = matE
      }
  where
    localExc = MolLogicException "sphericalToLocal"

----------------------------------------------------------------------------------------------------

-- | Data type for holding the best bond partners of a given atom. The given atom is always the
-- first one in the constructor, the others are the best bond partners found.
data BestBondPartners
  = Three (Int, Atom) (Int, Atom) (Int, Atom)
  | Two (Int, Atom) (Int, Atom)
  | One (Int, Atom)
  deriving (Show)

----------------------------------------------------------------------------------------------------

-- | Groups the atoms into groups of three non-colinear atoms within fragments. These groups are
-- allowed to overlap.
makeReferenceGroups ::
  (MonadThrow m, MonadIO m) =>
  -- | The whole layer for which to form groups.
  Molecule ->
  m (Seq BestBondPartners)
makeReferenceGroups mol = do
  -- Use a matching neighbourlist if already calculated or calculate a new one.
  nl <- do
    let matchingNLs = Map.lookupMin . Map.filterWithKey (\k _ -> k >= cutoffDistance) $ mol ^. #neighbourlist
    case matchingNLs of
      Nothing -> neighbourList cutoffDistance mol
      Just (_, nl) -> return nl

  -- Obtain groups within the fragments of the molecule.
  IntMap.foldl'
    ( \acc' f -> do
        acc <- acc'
        let fragAtoms = IntMap.restrictKeys (mol ^. #atoms) (f ^. #atoms)
            fragBonds = cleanBondMatByAtomInds (mol ^. #bonds) (f ^. #atoms)
        fragRefGroups <- groupsInFrag fragAtoms nl fragBonds
        return $ acc Seq.>< fragRefGroups
    )
    (pure Seq.Empty)
    (mol ^. #fragment)

----------------------------------------------------------------------------------------------------

-- | Build the local reference frame atom groups within the atoms and bonds of a fragment.
groupsInFrag ::
  MonadThrow m =>
  -- | Atoms in the current fragment only.
  IntMap Atom ->
  -- | The neighbourlist of the molecule with a matching search distance
  NeighbourList ->
  -- | The __intrafragment__ bond matrix. Must __not__ contain any bonds to atoms external to the
  -- current fragment.
  BondMatrix ->
  m (Seq BestBondPartners)
groupsInFrag atoms nl bondMat = do
  let dummyMol =
        Molecule
          { comment = mempty,
            atoms = atoms,
            bonds = bondMat,
            subMol = mempty,
            fragment = mempty,
            energyDerivatives = def,
            neighbourlist = mempty,
            calcContext = mempty,
            jacobian = Nothing
          }
  IntMap.foldlWithKey'
    ( \acc' k a -> do
        acc <- acc'
        bondPartnerKeys <- bondDistanceGroups dummyMol k 2
        let bondPartners = fmap (IntMap.toAscList . IntMap.restrictKeys atoms) bondPartnerKeys
        sph1PartnerIdx <- maybe2MThrow atomIdxExc $ bondPartners Seq.!? 1
        sph2PartnerIdx <- maybe2MThrow atomIdxExc $ bondPartners Seq.!? 2
        thisAtomsNeighbours <- maybe2MThrow atomIdxExc $ nl IntMap.!? k
        thisAtomsGroup <- selectCases atoms thisAtomsNeighbours (k, a) sph1PartnerIdx sph2PartnerIdx
        return $ acc |> thisAtomsGroup
    )
    (pure Seq.Empty)
    atoms
  where
    localExc = MolLogicException "groupsInFrag"
    atomIdxExc = localExc "Could not find atoms at index."

----------------------------------------------------------------------------------------------------

-- | This function obtains groups of 3 atoms to define a local coordinate system.
-- We follow a decission table to select what the best way for this atom is to define its
-- local group.
--
--   1. If there are at least 2 direct bond partners, that are not colinear with this
--      atom, use them.
--   2. If there is just 1 direct bond partner, but a second sphere bond partner exists,
--      we try to find a second sphere bond partner which gives a non-colinear triple.
--   3. If there is just 1 direct bond partner and no secon sphere bond partner, we use
--      the nearest neighbour of the first atom. This requires the neighbourlist.
--   4. If there is no bond partner at all we use the 2 nearest neighbours of this atom.
--      This requires a neighbourlist.
--   5. If there is just one or none neighbour in the neighbourlist 'Two' or 'One' will be returned.
selectCases ::
  MonadThrow m =>
  -- | All atoms in the current fragment.
  IntMap Atom ->
  -- | The neighbours of the current atom as obtained from a neighbourList.
  IntSet ->
  -- | The index of the current atom, which must be part of all atoms in the fragment.
  (Int, Atom) ->
  -- | Atoms directly bonded to the current one.
  [(Int, Atom)] ->
  -- | Atoms in the second sphere of bonds.
  [(Int, Atom)] ->
  m BestBondPartners
selectCases aif neigbourInds atomTuple@(_, atom) sph1Neighbours sph2Neighbours = do
  let -- Coordinates of the atom in question.
      coordsC = atom ^. #coordinates

      -- Vector of coordinates of all neighbours associated with the atom key.
      neigboursCoords :: Vector B (Vector S Double, Int)
      neigboursCoords =
        Massiv.fromList Seq
          . fmap swap
          . IntMap.toAscList
          . IntMap.map (getVectorS . (^. #coordinates))
          . IntMap.restrictKeys aif
          $ neigbourInds

  -- Distances to all neighbours associated with the atom keys.
  neighboursDistsOrdered :: Vector U (Double, Int) <-
    Massiv.quicksort
      <$> Massiv.traverseA
        ( \(c, k) -> do
            d <- distance (getVectorS coordsC) c
            return (d, k)
        )
        neigboursCoords

  -- Selecting the cases and entering recursion in a few colinear corner cases.
  case (sph1Neighbours, sph2Neighbours) of
    -- Case 1
    (x1 : x2 : xs, _) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          coordsX2 = x2 ^. _2 % #coordinates
      isLin <- isColinear coordsX1 coordsC coordsX2
      if isLin
        then selectCases aif neigbourInds atomTuple (x2 : xs) sph2Neighbours
        else return $ Three atomTuple x1 x2

    -- Case 2
    ([x1], y1 : ys) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          coordsY1 = y1 ^. _2 % #coordinates
      isLin <- isColinear coordsC coordsX1 coordsY1
      if isLin
        then selectCases aif neigbourInds atomTuple [x1] ys
        else return $ Three atomTuple x1 y1

    -- Case 3
    ([x1], []) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          nearestNeighbour = neighboursDistsOrdered !? 0
      case nearestNeighbour of
        Nothing -> return $ Two atomTuple x1
        Just (_, keyZ1) -> do
          atomZ1 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ1
          let coordsZ1 = atomZ1 ^. #coordinates
          isLin <- isColinear coordsX1 coordsC coordsZ1
          if isLin
            then selectCases (IntMap.delete keyZ1 aif) neigbourInds atomTuple [x1] []
            else return $ Three atomTuple x1 (keyZ1, atomZ1)

    -- Case 4
    ([], _) -> do
      let nearestNeighbour1 = neighboursDistsOrdered !? 0
          nearestNeighbour2 = neighboursDistsOrdered !? 1
      case (nearestNeighbour1, nearestNeighbour2) of
        (Just (_, keyZ1), Just (_, keyZ2)) -> do
          atomZ1 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ1
          atomZ2 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ2
          let coordsZ1 = atomZ1 ^. #coordinates
              coordsZ2 = atomZ2 ^. #coordinates
          isLin <- isColinear coordsZ1 coordsC coordsZ2
          if isLin
            then selectCases (IntMap.delete keyZ2 aif) neigbourInds atomTuple [] []
            else return $ Three atomTuple (keyZ1, atomZ1) (keyZ2, atomZ2)
        (Just (_, keyZ1), Nothing) -> do
          atomZ1 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ1
          return $ Two atomTuple (keyZ1, atomZ1)
        _ -> return $ One atomTuple
  where
    localExc = MolLogicException "selectCases"
    atomIdxExc = localExc "Could not find atom in fragment."

    -- Checks if three atoms are colinear.
    isColinear :: MonadThrow m => VectorS Double -> VectorS Double -> VectorS Double -> m Bool
    isColinear a b c = do
      ab <- getVectorS a .-. getVectorS b
      bc <- getVectorS b .-. getVectorS c
      alpha <- angle ab bc
      return . not $ alpha <= (179 / 360) * 2 * pi && alpha >= (1 / 360) * 2 * pi

----------------------------------------------------------------------------------------------------

-- | Construction of local axes system from 'BestBondPartners' data. In case of 'Three' the central
-- atom is the first one. Builds for all atoms a local axes system. Returns a small 'IntMap' with
-- the atom keys associated to their axes system.
makeLocalAxesSystemE :: MonadThrow m => BestBondPartners -> m (IntMap (Matrix S Double))
makeLocalAxesSystemE bbp =
  let unitMat3x3 = computeS . identityMatrix $ Sz 3
   in case bbp of
        One (k, _) -> return $ IntMap.singleton k unitMat3x3
        Two (kB, aB) (kA, aA) -> do
          -- Get the coordinates of the atoms.
          let rA = getVectorS $ aA ^. #coordinates
              rB = getVectorS $ aB ^. #coordinates

          -- Calculate the axes from the reference system. The y-axis is freely chosen to be
          -- orthogonal to the z-axis and the x-axis is the normalised cross product of them.
          zAB <- (rA .-. rB) >>= \rAB -> pure $ rAB .* (1 / magnitude rAB)
          zABx <- zAB !? 0
          zABy <- zAB !? 1
          zABz <- zAB !? 2
          let yABNoNorm = Massiv.fromList @S Seq [1, (- zABx - zABz) / zABy, 1]
              yAB = yABNoNorm .* (1 / magnitude yABNoNorm)
          xAB :: Vector S Double <- zAB `cross3` yAB >>= \cp -> pure $ cp .* (1 / magnitude cp)

          -- Construct the axes matrices.
          axesA <- coordsToAxesMat [xAB, yAB, zAB]
          axesB <- return axesA

          -- Construct the IntMap associations between atoms and their axes systems.
          return . IntMap.fromList $ [(kA, axesA), (kB, axesB)]
        Three (kB, aB) (kA, aA) (kC, aC) -> do
          -- Get the coordinates of the atoms.
          let rA = getVectorS $ aA ^. #coordinates
              rB = getVectorS $ aB ^. #coordinates
              rC = getVectorS $ aC ^. #coordinates

          -- Calculate the axes from the reference atom coordinates.
          zAB <- (rA .-. rB) >>= \rAB -> pure $ rAB .* (1 / magnitude rAB)
          zC <- (rB .-. rC) >>= \rBC -> pure $ rBC .* (1 / magnitude rBC)
          yABC <- zAB `cross3` zC >>= \cp -> pure $ cp .* (1 / magnitude cp)
          xAB <- zAB `cross3` yABC
          xC <- zC `cross3` yABC

          -- Construct the axes matrices.
          axesA <- coordsToAxesMat [xAB, yABC, zAB]
          axesB <- return axesA
          axesC <- coordsToAxesMat [xC, yABC, zC]

          -- Construct the IntMap associations between atoms and axes systems.
          return . IntMap.fromList $ [(kA, axesA), (kB, axesB), (kC, axesC)]
  where
    coordsToAxesMat :: (Foldable f, MonadThrow m) => f (Vector S Double) -> m (Matrix S Double)
    coordsToAxesMat vecs =
      concatM 1 vecs
        >>= resizeM (Sz $ 3 :. 3)
        >>= pure . computeS @S . transpose . computeS @S
