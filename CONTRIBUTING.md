# Contributing

## Source formatting
### Documentation and Commenting
Every top level function and declaration needs to be documented.
Use [Haddock](https://www.haskell.org/haddock/) to write visible documentation for every function, where assumptions and behaviour of the function are clearly described.
In case there could be any ambiguities what the arguments of a function mean, also document them using Haddock.
Top level functions documenting should use Haddock style block comments with the comment opening and closing braces in extra lines.

Local declarations in `let` or `where` should be documented if it is not absolutely clear, what their meaning is. Use normal (non-Haddock) comments for this.

If you use any abbreviations, that are not immediately obvious, put a comment there, so that the function is easily understandable.

__*Example:*__
```haskell
{-|
Take a group ('Seq') of 'Atom's as formed by 'groupAsSubMols and the bonds of the complete
'Molecule'. Then form a proper submolecule. This function relies on the group of being a proper
submolecule and will hapilly accept groups that are not.
-}
makeSubMolFromGroup
  :: Seq (Int, (Int, TL.Text), Atom) -- ^ A group of annotated atoms in the style of:
                                     --   @(atomIndex, (subMolID, subMolName), atom)@
  -> IntMap IntSet                   -- ^ Bond type data structure of the whole molecule.
  -> Molecule                        -- ^ A newly formed submolecule.
makeSubMolFromGroup group bonds =
  let atoms    = IM.fromList . toList . fmap (\(ind, _, atom) -> (ind, atom)) $ group
      label    = fromMaybe "" . (S.!? 0) . fmap (^. _2 . _2) $ group
      atomInds = IM.keysSet atoms
      -- Remove all bonds from the IntMap, that have origin on atoms not in this set and all
      -- target atoms that are not in the set.
      bondsCleaned :: IntMap IntSet
      bondsCleaned = IM.map (IS.filter (`IS.member` atomInds)) $ bonds `IM.restrictKeys` atomInds
  in  Molecule { _molecule_Label    = label
               , _molecule_Atoms    = atoms
               , _molecule_Bonds    = bondsCleaned
               , _molecule_SubMol   = S.empty
               , _molecule_Energy   = Nothing
               , _molecule_Gradient = Nothing
               , _molecule_Hessian  = Nothing
               }

  ```

### Function Declaration and Indentation
Line width maximum is 100 characters, both for comments and for code.
Generally functions should not be longer than 30 lines (excluding comments and possibly multiline records).
If a function cannot be expressed in 30 lines, split it into atomic functions. Short functions are easier to understand, debug and reuse.
Exceptions might be parsers and writers for text processing or wrapper interactions in a process.

Code needs to be formatted with `brittany` and the supplied `brittany.yaml` files needs to be used.
Imports and modules should be cleaned with `stylish-haskell`.
Therefore clean first with `stylish-haskell` if you changed imports or language pragmas, and with `brittany` afterwards.

If you need local functions, define them in a `where` block, for the function, where you need them.
`where` blocks must not be nested.
Use `let`/`let ... in` constructs to assign local variable names.

### Other
Do not use ***any*** [partial functions](https://wiki.haskell.org/Partial_functions) in Spicy, neither from `Prelude` or libraries, nor self-written -- no matter which justification you might think you have.

Non-IO exceptions should be reported by `MonadThrow` with proper error types.
Exception types might be defined in `Spicy.Types` and made instances of `Exception`.
If an exception occurs, the corresponding exception type should be thrown with `throwM` from [`Contro.Exception.Safe`](https://hackage.haskell.org/package/safe-exceptions).
Generally, for error handling follow the [ideas from FPComplete](https://tech.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell).

To allow for composable parser errors, use `parse'` instead of Attoparsec's `parse`.

## Testing and Benchmarking
For all major functions (all that are exported in a module) there must be a unit or golden test. *All* tests muss pass. Tests are in written in the [Tasty](http://hackage.haskell.org/package/tasty) framework, both for unit tests and golden tests.
Tests should be performed by a simple `stack test`.

Spicy shall be able to handle large molecules up to protein size. Use [Criterion](http://hackage.haskell.org/package/criterion) benchmarks for functions, that need to handle large data structures and test for different input sizes, to check if they scale and parallelise well. Benchmarks can be executed by `stack bench`, but this can cause problems with parallel functions. Better build the executable and run it standalone as `stack exec benchmarks`.


## Git
Spicy uses the GitFlow model, see [here](https://medium.com/@nuno.caneco/using-git-flow-243581525aee) for an introduction.
The `develop`, `master` and `release` branches are protected by GitLab, with the former two requiring CI-tests to pass and reviewed pull-requests before merging, while `release` might get non pull request commits from administrators.

Merging is only possible via merge commits. It is OK to `squash` a branche while mergin.
On the other hand side, to keep e.g. feature branches up to date, use `rebase` only.
