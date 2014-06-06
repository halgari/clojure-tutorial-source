Core.Async Episode 7


puts -> buffer -> takes

puts are [callbacks values]
buffer is a queue of values
takes are a queue of callbacks

puts or takes, not both


go blocks - a set of callbacks and a state machine
a go block can be in two places
1) attached to a channel
2) in a thread-pool (executing or not)


(go (let [v (<! c)      ; attach to c
          v2 (inc v)]   ; in thread-pool
     (>! c2 v2)))       ; attach to c2

1) attach to c via take! - go is a callback on c
1.1) c is GC'd then the go will be GC'd
2) go is put into the thread-pool with the value from c
3) attach to c2 via put! - go is a callback on c2


c -> callback -> state-machine
c2 -> [callback value] -> state-machine

state-machine is all the local state of the go

gos are 3 things:
1) a state-machine
2) a callback
3) a return-channel

callback -> state-machine -> return-channel

(def foo (let [c (chan)]
           (go (>! c 42))))

1) create c
2) go tries to put onto c and parks
3) go returns a return-channel
4) return-channel assigned to foo
5) GC happens....
6) c is GC'd
7) the go is GC'd
8) return-channel is in foo


(go (loop []
      (<! (timeout 42))
      (recur)))

timeout queue -> timeout-c -> go


(let [c (chan)
      c2 (chan)
      c3 (chan)]
 (go (<! c)
     (>! c2 42))
 (go (<! c2)
     (>! c3 42))
 (go (<! c3)
     (>! c 42)))


 (let [c (chan)]
   (thread (<!! c)))



 (let [c (chan (dropping-buffer 1))]
   (go (loop []
          (>! c 42)
          (recur))))





 1) Think about who owns the top level values
 2) Think about how to shut those down





