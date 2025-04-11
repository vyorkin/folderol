let rec accumulate f xs =
  match xs with [], y -> y | x :: xs', y -> accumulate f (xs', f (x, y))

