module Message (
  Message(..)
) where


data Message
  = Ignore
  | ReadAnswer String
  | TrySolve String
