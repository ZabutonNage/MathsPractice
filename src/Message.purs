module Message (
  Message(..)
) where


data Message
  = Ignore
  | ChangeGameMode String
  | ReadLimit String
  | StartLimit
  | StopLimit
  | ReadAnswer String
  | TrySolve String
