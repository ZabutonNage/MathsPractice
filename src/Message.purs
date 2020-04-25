module Message (
  Message(..)
) where


data Message
  = Ignore
  | ReadInput String
  | TrySolve String
