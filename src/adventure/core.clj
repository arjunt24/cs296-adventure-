(ns adventure.core)
(require '[clojure.string :as str])


(def init-map
  {:room0 {:desc "The zeroth room"
           :title "Room 0"
           :dir {:north :room1 :east :room2 :south :room3 :west :room4}
           :contents #{:object0}}
   :room1 {:desc "The first room"
            :title "Room 1"
            :dir {:north :room5 :south :room0}
            :contents #{:object1}}
   :room2 {:desc "The second room"
            :title "Room 2"
            :dir {:east :room6 :west :room0}
            :contents #{:object2}}
   :room3 {:desc "The third room"
            :title "Room 3"
            :dir {:south :room7 :north :room0}
            :contents #{:object3}}
   :room4 {:desc "The fourth room"
            :title "Room 4"
            :dir {:west :room8 :east :room0}
            :contents #{:object4}}
    :room5 {:desc "The fifth room"
             :title "Room 5"
             :dir {:south :room1}
             :contents #{:object5}}
    :room6 {:desc "The sixth room"
             :title "Room 6"
             :dir {:west :room2}
             :contents #{:object6}}
    :room7 {:desc "The seventh room"
             :title "Room 7"
             :dir {:north :room3}
             :contents #{:object7}}
    :room8 {:desc "The eighth room"
             :title "Room 8"
             :dir {:east :room4}
             :contents #{:object8}}
  })

(def init-items
    {:object0 {:desc "The zeroth obj"
               :name "Object0" }
    :object1 {:desc "The first obj"
               :name "Object1" }
    :object2 {:desc "The second obj"
               :name "Object2" }
    :object3 {:desc "The third obj"
               :name "Object3" }
    :object4 {:desc "The four obj"
               :name "Object4" }
    :object5 {:desc "The five obj"
               :name "Object5" }
    :object6 {:desc "The six obj"
               :name "Object6" }
    :object7 {:desc "The seven obj"
               :name "Object7" }
    :object8 {:desc "The eight obj"
               :name "Object8" }
    :objectwin {:desc "The victory obj"
               :name "ObjectWin" }

  })

(def init-adventurer
    {:location :room0
     :inventory #{}
     :key3 "val"
     :key4 "val"
     :tick 0
     :seen #{:room0}})

(def init-state {:map init-map :items init-items :adventurer init-adventurer})


(defn printState [state]
  (println "map: ")
    ;(println (get-in state [:map]))
    (def rooms [:room0 :room1 :room2 :room3 :room4 :room5 :room6 :room7 :room8])
    ;(println (range (count rooms)))
    (doseq [i (range (count rooms))]
      ;(println "hello "))
      ;(def room-attributes [:desc :title :dir :contents])
      (println "  " (nth rooms i) "  " (get-in state [:map (nth rooms i)])))
  (println "adventurer: ")
    (def adv-attributes [:location :inventory :key3 :key3 :tick :seen])
    (doseq [i (range (count adv-attributes))]
      (println "  " (nth adv-attributes i) "  " (get-in state [:adventurer (nth adv-attributes i)])))
  (println "items: ")
    (def items [:obj0 :obj1 :obj2 :obj3 :obj4 :obj5 :obj6 :obj7 :obj8])
    (doseq [i (range (count items))]
      (println "  " (nth items i) "  " (get-in state [:items (nth items i)]))))


(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (let[new-state  (assoc-in state [:adventurer :location] dest)]
        (if-let [val (get-in state [:adventurer :seen dest])]
          (println (get-in state [:map dest :title]))
          (println (get-in state [:map dest :desc])))
      (update-in (update-in new-state [:adventurer :seen] #(conj % dest)) [:adventurer :tick] #(inc %))))))

(defn examine [state item]
  (if-let [val (get-in state [:map (get-in state [:adventurer :location]) :contents item])]
    (println (get-in state [:items item :desc]))
    (if-let [val (get-in state [:adventurer :inventory item])]
      (println (get-in state [:items item :desc]))
      (println "item not in room or inventory")))
  state)

(defn look [state]
  (do
    (print (get-in state [:map (get-in state [:adventurer :location]) :desc]))
    (let [items (vec (get-in state [:map (get-in state [:adventurer :location]) :contents]))]
    (print ". Items: ")
      (if (= (count items) 0)
        (print "none.")
      (doseq [i (range (count items))]
        (print (get-in init-state [:items (items i) :name]))
        (if (= i (- (count items) 1))
          (print ". ")
          (print ", ")))))
    (let [dirs (vec (get-in state [:map (get-in state [:adventurer :location]) :dir]))]
    (print "Paths: ")
      (if (= (count dirs) 0)
        (println "none.")
      (doseq [i (range (/ (count dirs) 1))]
        (if (= (+ i 1) (count dirs))
          (println (name ((dirs i) 0)) (str "- "(name ((dirs i) 1)) ". "))
          (print (name ((dirs i) 0)) (str "- "(name ((dirs i) 1)) ", "))
          ))))
  state))

(defn inventory [state]
  (let [items (vec (get-in state [:adventurer :inventory]))]
    (if (= (count items) 0)
      (println "Your inventory is empty.")
      (do
      (print "inventory: ")
      (doseq [i (range (count items))]
        (print (get-in init-state [:items (items i) :name]))
        (if (= i (- (count items) 1))
          (println ".")
          (print ", ")
        ))))
  state))

(defn take [state item]
  (inventory
  (if-let [val (get-in state [:map (get-in state [:adventurer :location]) :contents item])]
    (let [new-state (update-in state [:adventurer :inventory] #(conj % item))]
      (let [newnew-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(disj % item))]
        ;(printState newnew-state)
        newnew-state))
  state)))

(defn drop [state item]
  (inventory
  (if-let [val (get-in state [:adventurer :inventory item])]
    (let [new-state (update-in state [:adventurer :inventory] #(disj % item))]
      (let [newnew-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(conj % item))]
          newnew-state))
  state)))

(defn actually-transform [state]
  (def new-state (update-in state [:adventurer :inventory] #(conj % :objectwin)))
  (def new-new-state (update-in new-state [:adventurer :inventory] #(disj % :object8)))
  (println "Object8 has transformed into ObjectWin!")
  new-new-state
)

(defn transform [state item]
  (if (= item :object8)
    (do
      (if-let [val (get-in state [:adventurer :inventory :object8])]
        (if (= :room5 (get-in state [:adventurer :location]))
          (def new-state (actually-transform state))
          (do (print "you can't transform here") (def new-state state)))
        (do (print "you dont have object 8") (def new-state state)))
        new-state
      )
    (do
      (print "you can't transform that!")
      state
      ))
)

(defn help [state]
  (println "type 'go north' or 'n' to go north")
  (println "type 'look' to look around")
  (println "type 'take obj' to pick up an object")
  (println "type 'drop obj' to drop an object")
  (println "type 'i' to look at inventory")
  (println "type 'transform obj' to try to transform the object")
  (println "goal of the game: find object8, take it to room5, transform it to ObjectWin, then place ObjectWin in room7")

  state)


(defn match [pattern input]
  (loop [pattern pattern
    input input
    vars '()]
  (cond (and (empty? pattern) (empty? input)) (reverse vars)
    (or (empty? pattern) (empty? input)) nil
      (= (first pattern) "@")
        (recur (rest pattern)
        (rest input)
    (cons (first input) vars))
      (= (first pattern) (first input))
        (recur (rest pattern)
        (rest input)
      vars)
      :fine-be-that-way nil )))

(defn base [state]
	   [["@"]
        (fn [x]
          (if (or (= x "n") (= x "north")) (go state :north)
          (if (or (= x "e") (= x "east" )) (go state :east)
          (if (or (= x "s") (= x "south")) (go state :south)
          (if (or (= x "w") (= x "west" )) (go state :west)
          (if (= x "look") (look state)
          (if (= x "help") (help state)
          (if (or (= x "i") (= x "inventory")) (inventory state)
          (if (= x "print") (do (printState state) state)
          (let [ret "invalid input"] (println ret) state))))))))))
     ["go" "@"]
        (fn [x] (go state (keyword x)))
     ["examine" "@"]
        (fn [x] (examine state (keyword x)))
     ["take" "@"]
        (fn [x] (take state (keyword x)))
     ["drop" "@"]
        (fn [x] (drop state (keyword x)))
     ["transform" "@"]
        (fn [x] (transform state (keyword x)))])


(defn react [state baseinput]
  (let [canon-input (vec (str/split (str/replace (str/lower-case baseinput) #"[?.!]" "") #" +"))]
	(loop [idx 0]
  	(if (>= idx (count (base state)))
      (let [ret "invalid input"]
        (println ret)
        state)
    	(if-let [vars (match ((base state) idx) canon-input)]
      	(apply ((base state) (inc idx)) vars)
      	(recur (+ idx 2)))))))

(defn repl
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
  (if-let [val (get-in local-state [:map :room7 :contents :objectwin])]
    (println (str "\nYou win! You finished in " (get-in local-state [:adventurer :tick]) " moves."))

    (do (print (str "\nYou are at " (get-in local-state [:map (get-in local-state [:adventurer :location]) :title]) ". "))
    (let [
          _  (println "What do you want to do?")
          command (read-line)]
      (if (= command "quit") (println "bye!")
        (let [new-state (react local-state command)]
        (recur new-state))))))))
