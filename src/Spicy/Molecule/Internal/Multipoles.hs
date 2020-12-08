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
-- \[
--
-- \]
module Spicy.Molecule.Internal.Multipoles
  ( OctahedralModel (..),
    toOctrahedralModel,
    octahedronToLocalFrame,
    sphericalToLocal,
  )
where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Massiv.Array as Massiv hiding (swap)
import Data.Tuple (swap)
import Optics hiding (Empty)
import RIO hiding (Vector, lens, view, (%~), (.~), (^.), (^?))
import qualified RIO.HashMap as HashMap
import RIO.Seq (Seq (..))
import qualified RIO.Seq as Seq
import Spicy.Common
import Spicy.Data
import Spicy.Math
import Spicy.Molecule.Internal.Types hiding (S, coordinates)
import Spicy.Molecule.Internal.Util

data OctahedralValues = OctahedralValues
  { -- | Positive X
    qXu :: !Double,
    -- | Negative X
    qXd :: !Double,
    -- | Positive Y
    qYu :: !Double,
    -- | Negative Y
    qYd :: !Double,
    -- | Positive Z
    qZu :: !Double,
    -- | Negative Z
    qZd :: !Double
  }
  deriving (Eq, Show)

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qXu s) $ \s b -> s {qXu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qXd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qXd s) $ \s b -> s {qXd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qYu s) $ \s b -> s {qYu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qYd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qYd s) $ \s b -> s {qYd = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZu" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qZu s) $ \s b -> s {qZu = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "qZd" k OctahedralValues OctahedralValues a b where
  labelOptic = lens (\s -> qZd s) $ \s b -> s {qZd = b}

----------------------------------------------------------------------------------------------------

-- | The multipoles can be converted to an octahedral point charge model. Those charges might be
-- defined with respect to the principal axis of the spherical quadrupole tensor or in a local
-- reference frame of the molecule.
data OctahedralModel
  = SphericalRef
      { -- | The values of the point charges in the corners of the octahedron in the given axes system.
        values :: !OctahedralValues,
        -- | The local axes system.
        axesSystem :: !(Matrix S Double),
        -- | The \(d_q\) value in Angstrom.
        dq :: !Double,
        -- | The coordinates of the 6 point charges given in order (see 'OctahedralValues') as
        -- \(3 \times 6\) matrix with the coordinates given as column vectors of the matrix.
        -- | These are the global cartesian coordinates in angstrom (same as for 'Molecule').
        coordinates :: !(Maybe (Matrix S Double))
      }
  | LocalRef
      { -- | The values of the point charges in the corners of the octahedron in the given axes system.
        values :: !OctahedralValues,
        -- | The local axes system.
        axesSystem :: !(Matrix S Double),
        -- | The \(d_q\) value in Angstrom.
        dq :: !Double,
        -- | The coordinates of the 6 point charges given in order (see 'OctahedralValues') as
        -- \(3 \times 6\) matrix with the coordinates given as column vectors of the matrix.
        -- | These are the global cartesian coordinates in angstrom (same as for 'Molecule').
        coordinates :: !(Maybe (Matrix S Double))
      }

-- Lenses
instance (k ~ A_Lens, a ~ OctahedralValues, b ~ a) => LabelOptic "values" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> values s) $ \s b -> s {values = b}

instance (k ~ A_Lens, a ~ Matrix S Double, b ~ a) => LabelOptic "axesSystem" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> axesSystem s) $ \s b -> s {axesSystem = b}

instance (k ~ A_Lens, a ~ Double, b ~ a) => LabelOptic "dq" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> dq s) $ \s b -> s {dq = b}

instance (k ~ A_Lens, a ~ Maybe (Matrix S Double), b ~ a) => LabelOptic "coordinates" k OctahedralModel OctahedralModel a b where
  labelOptic = lens (\s -> coordinates s) $ \s b -> s {coordinates = b}

isSphericalRef :: OctahedralModel -> Bool
isSphericalRef ref = case ref of
  SphericalRef {} -> True
  _ -> False

----------------------------------------------------------------------------------------------------

-- | Converts the atomic multipoles to the octahedral charge model in the coordinate system of the
-- spherical quadrupole tensor. All moments higher than quadrupole will be neglected.
toOctrahedralModel ::
  -- | The distance from the origin \(d_q\) in Angstrom. Default to \( \frac{1}{4} a_0 \)
  Maybe Double ->
  -- | The multipoles to convert.
  Multipoles ->
  -- | The octahedral point charge representation of the multipoles in the principla axis of the
  -- spherical quadrupole tensor.
  OctahedralModel
toOctrahedralModel dist mp =
  let d = fromMaybe (bohr2Angstrom 0.25) (bohr2Angstrom <$> dist)
      q00 = fromMaybe 0 $ mp ^? #monopole % _Just % #q00
      q10 = fromMaybe 0 $ mp ^? #dipole % _Just % #q10
      q11c = fromMaybe 0 $ mp ^? #dipole % _Just % #q11c
      q11s = fromMaybe 0 $ mp ^? #dipole % _Just % #q11s
      q20 = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q20
      q22c = fromMaybe 0 $ mp ^? #quadrupole % _Just % #q22c
      e2 = 2 :: Int
   in SphericalRef
        { axesSystem = Massiv.makeArray Seq (Sz (3 :. 3)) $ \(i :. j) -> if i == j then 1 else 0,
          dq = d,
          coordinates = Nothing,
          values =
            OctahedralValues
              { qXu = (q00 / 6) + (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2)),
                qYd = (q00 / 6) - (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2)),
                qXd = (q00 / 6) - (q11c / (2 * d)) - (q20 / (6 * d ^ e2)) + (q22c / (2 * sqrt (3) * d ^ e2)),
                qZu = (q00 / 6) + (q10 / (2 * d)) + (q20 / (3 * d ^ e2)),
                qYu = (q00 / 6) + (q11s / (2 * d)) - (q20 / (6 * d ^ e2)) - (q22c / (2 * sqrt (3) * d ^ e2)),
                qZd = (q00 / 6) - (q10 / (2 * d)) + (q20 / (3 * d ^ e2))
              }
        }

----------------------------------------------------------------------------------------------------

-- | Transformation of an octahedral charge model to the local reference frame defined by bonds.
-- Although in case of a non bonded atom the multipole orientation *should* not matter to much, this
-- function will fall back to define two bonds to arbitrarily chosen atoms. This should allow to
-- treat polarisation effects of monoatomic ions in solution or something like this.
octahedronToLocalFrame :: IntMap Atom -> BondMatrix -> IntMap OctahedralModel
octahedronToLocalFrame atoms bondMat =
  let
   in undefined

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
--      d_q & -d_q & 0   &    0 &   0 &    0 \\
--        0 &    0 & d_q & -d_q &   0 &    0 \\
--        0 &    0 &   0 &    0 & d_q & -d_q
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
  unless (isSphericalRef sphModel) . throwM . localExc $
    "Input reference system is not spherical."

  let matP = sphModel ^. #axesSystem
      matPT = computeAs S . setComp Seq . transpose $ matP
      dq = sphModel ^. #dq

  -- The 3 x 6 matrix with the octahedral coordinates. The coordinates appear as defined above
  -- and are column vectors.
  {- ORMOLU_DISABLE -}
  (matC :: Matrix S Double) <- Massiv.fromListsM Seq
    --  Xu,  Xd,  Yu,  Yd, Zu, Zd
    [ [ dq, -dq,   0,   0,  0,   0]
    , [  0,   0,  dq, -dq,  0,   0]
    , [  0,   0,   0,   0, dq, -dq]
    ]
  {- ORMOLU_ENABLE -}

  -- The matrix equation applied.
  matC' <- matPT .><. matE >>= \pe -> (computeAs S pe) .><. matC

  return $
    LocalRef
      { values = sphModel ^. #values,
        axesSystem = matE,
        dq = dq,
        coordinates = Just . computeS $ matC'
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

-- | Groups the atoms into groups of three non-colinear atoms within fragments. These groups are
-- allowed to overlap.
makeGroups ::
  (MonadThrow m, MonadIO m) =>
  -- | The whole layer for which to form groups.
  Molecule ->
  -- | The neighbourlist of this group if already calculated.
  Maybe (IntMap IntSet) ->
  m (IntMap BestBondPartners)
makeGroups mol nL = do
  newNL <- neighbourList 10.0 mol
  let atoms = mol ^. #atoms
      frags = mol ^. #fragment
      bondMat = mol ^. #bonds
      actualNL = fromMaybe newNL nL

  return undefined
  where
    localExc = MolLogicException "makeGroups"

-- | Grouping within a fragment.
groupInFrag :: MonadThrow m => IntMap Atom -> IntSet -> BondMatrix -> IntMap IntSet -> m (Seq BestBondPartners)
groupInFrag atoms frags bonds nL = do
  let atomsInFrag = IntMap.restrictKeys atoms frags
      bondsInFrag = cleanBondMatByAtomInds bonds frags
  groups <- case IntMap.size atomsInFrag of
    0 -> throwM . localExc $ "No atoms in fragment."
    1 -> return . Seq.singleton . One . IntMap.findMin $ atomsInFrag
    2 -> return . Seq.singleton $ Two (IntMap.findMin atomsInFrag) (IntMap.findMax atomsInFrag)
    _ -> undefined
  return groups
  where
    localExc = MolLogicException "makeGroups"

data BondCase = NonTerminal | Terminal | TerminalColinear

-- | Folding into overlapping groups of atoms within a fragment.
groupsOfThree :: MonadThrow m => IntMap Atom -> BondMatrix -> IntMap IntSet -> m (Seq BestBondPartners)
groupsOfThree atoms bonds nL = do
  let dummyMol =
        Molecule
          { comment = mempty,
            atoms = atoms,
            bonds = bonds,
            subMol = mempty,
            fragment = mempty,
            energyDerivatives = def,
            calcContext = mempty,
            jacobian = Nothing
          }
  groups <- do
    IntMap.foldlWithKey'
      ( \acc k a -> do
          -- Get the bond partners of the current atom.
          bondPartners <- bondDistanceGroups dummyMol k 2
          directPartners <- maybe2MThrow (localExc "Too few bonds specified") $ bondPartners Seq.!? 1
          secondSphPartners <- maybe2MThrow (localExc "Too few bonds specified") $ bondPartners Seq.!? 2
          return undefined
      )
      (pure Seq.empty)
      atoms
  return undefined
  where
    localExc = MolLogicException "makeGroups"

-- | This function obtains groups of 3 atoms to define a local coordinate system.
-- We follow a decission table to select what the best way for this atom is to define its
-- local group.
--   1. If there are at least 2 direct bond partners, that are not colinear with this
--      atom, use them.
--   2. If there is just 1 direct bond partner, but a second sphere bond partner exists,
--      we try to find a second sphere bond partner which gives a non-colinear triple.
--   3. If there is just 1 direct bond partner and no secon sphere bond partner, we use
--      the nearest neighbour of the first atom. This requires the neighbourlist.
--   4. If there is no bond partner at all we use the 2 nearest neighbours of this atom.
--      This requires a neighbourlist.
--      4.1. If there is just one or none neighbour in the neighbourlist 'Two' or 'One' will be
--           returned.
selectCases ::
  MonadThrow m =>
  -- | All atoms in the current fragment.
  IntMap Atom ->
  -- | The neighbours of the current atom as obtained from a neighbourList.
  IntSet ->
  -- | The index of the current atom, which must be part of all atoms in the fragment.
  (Int, Atom) ->
  -- | Atoms directly bonded to the current one.
  Seq (Int, Atom) ->
  -- | Atoms in the second sphere of bonds.
  Seq (Int, Atom) ->
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
    (x1 :<| x2 :<| xs, _) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          coordsX2 = x2 ^. _2 % #coordinates
      isLin <- isColinear coordsX1 coordsC coordsX2
      if isLin
        then selectCases aif neigbourInds atomTuple (x2 :<| xs) sph2Neighbours
        else return $ Three atomTuple x1 x2

    -- Case 2
    (x1 :<| Empty, y1 :<| ys) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          coordsY1 = y1 ^. _2 % #coordinates
      isLin <- isColinear coordsC coordsX1 coordsY1
      if isLin
        then selectCases aif neigbourInds atomTuple (Seq.singleton x1) ys
        else return $ Three atomTuple x1 y1

    -- Case 3
    (x1 :<| Empty, Empty) -> do
      let coordsX1 = x1 ^. _2 % #coordinates
          nearestNeighbour = neighboursDistsOrdered !? 0
      case nearestNeighbour of
        Nothing -> return $ Two atomTuple x1
        Just z1@(_, key) -> do
          atomZ1 <- maybe2MThrow atomIdxExc $ aif ^? ix key
          let coordsZ1 = atomZ1 ^. #coordinates
          isLin <- isColinear coordsX1 coordsC coordsZ1
          if isLin
            then selectCases (IntMap.delete key aif) neigbourInds atomTuple (Seq.singleton x1) Empty
            else return $ Three atomTuple x1 (key, atomZ1)

    -- Case 4
    (Empty, _) -> do
      let nearestNeighbour1 = neighboursDistsOrdered !? 0
          nearestNeighbour2 = neighboursDistsOrdered !? 1
      case (nearestNeighbour1, nearestNeighbour2) of
        (Just z1@(_, keyZ1), Just z2@(_, keyZ2)) -> do
          atomZ1 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ1
          atomZ2 <- maybe2MThrow atomIdxExc $ aif ^? ix keyZ2
          let coordsZ1 = atomZ1 ^. #coordinates
              coordsZ2 = atomZ2 ^. #coordinates
          isLin <- isColinear coordsZ1 coordsC coordsZ2
          if isLin
            then selectCases (IntMap.delete keyZ2 aif) neigbourInds atomTuple Empty Empty
            else return $ Three atomTuple (keyZ1, atomZ1) (keyZ2, atomZ2)
        (Just z1@(_, keyZ1), Nothing) -> do
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
