module Queue where

data Queue a
  = Queue
      { enqueue :: [a],
        dequeue :: [a]
      }
  deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x Queue {enqueue = ex, dequeue = dx} = Queue {enqueue = x : ex, dequeue = dx}

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop Queue {enqueue = xs, dequeue = []} = pop $ Queue {enqueue = [], dequeue = reverse xs}
pop Queue {enqueue = ex, dequeue = (d : dx)} = Just (d, Queue {enqueue = ex, dequeue = dx})
