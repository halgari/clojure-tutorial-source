(ns intro-to-csp.episode1)

(comment

(-> (create-frame)
    (add-body)
    (map (add-tire) (make-tire 5))
    (add-engine)
    (ship-car))


(create-frame)
|
v
(add-body)
|          (make-tires 5)
|           | = buffer size = 6000
v           v
(combine-tires-with-car)
|
v
(add-engine)
|
v
(ship-car)


CSP

processes = go blocks or threads
queues = channels

channels = buffers (0-n)
dropping buffer = drops newest inputs
sliding buffer = drops oldest inputs
