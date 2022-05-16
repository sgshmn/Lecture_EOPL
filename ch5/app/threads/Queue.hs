module Queue where

type Queue a = [a]

empty_queue :: Queue a
empty_queue = []

isempty :: Queue a -> Bool
isempty queue = null queue

enqueue :: Queue a -> a -> Queue a
enqueue queue elem = queue ++ [elem]

dequeue :: Queue a -> (a -> Queue a -> b) -> b 
dequeue queue f = f (head queue) (tail queue)
  -- b = Store -> SchedState -> (FinalAnswer, Store)
