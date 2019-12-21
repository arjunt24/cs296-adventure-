(ns adventure.core)
(require '[clojure.string :as str])


(def init-map
  {:akron {:desc "Your hometown and birthplace. It feels like ultimately, this is where you belong."
           :title "Akron"
           :dir {:north :springfield :east :cleveland :south :miami :west :los-angeles}
           :contents #{:top-draft-pick}}
   :springfield {:desc "The birthplace of basketball, Springfield is steeped in basketball history. There is a very ominous feeling..."
            :title "Springfield"
            :dir {:south :akron };:north :hall-of-fame}
            :contents #{}}
   :cleveland {:desc "Your hometown Cleveland Cavaliers. You will always have a home here; this team will always be all yours."
            :title "Cleveland"
            :dir {:east :nba-awards-show :west :akron}
            :contents #{:scoring-title}}
   :miami {:desc "It's time to get serious and win a ring; you decide to team up with other superstars on the Miami Heat."
            :title "Miami"
            :dir {:south :nba-finals :north :akron}
            :contents #{:first-seed}}
   :los-angeles {:desc "You were always meant for the lights and glamour; it's time to shine with the LA Lakers, the greatest franchise in the NBA."
            :title "Los-Angeles"
            :dir {:west :staples-center :east :akron}
            :contents #{:rebounding-title :assists-title}}
    :hall-of-fame {:desc "The Naismith Memorial Basketball Hall of Fame celebrates the greatest basketball players of history - like you."
             :title "Hall-of-Fame"
             :dir {:south :springfield}
             :contents #{}}
    :nba-awards-show {:desc "The annual showcase of the best NBA players from the last year."
             :title "NBA-Awards-Show"
             :dir {:west :cleveland}
             :contents #{:mvp}}
    :nba-finals {:desc "It's the moment of truth: the NBA Finals."
             :title "NBA-Finals"
             :dir {:north :miami}
             :contents #{:championship-ring :finals-mvp}}
    :staples-center {:desc "The historic and spectacular home of the LA Lakers."
             :title "Staples Center"
             :dir {:east :los-angeles}
             :contents #{:all-nba-first-team}}
  })

(def init-items
    {:scoring-title {:desc "The title given to the most prolific scorer of the season, very few have the honor of holding this title"
               :name "Scoring-Title" }
    :assists-title {:desc "The title given to the most generous playmaker of the season, very few have the honor of holding this title"
               :name "Assists-Title" }
    :rebounding-title {:desc "The title given to the king of the boards, very few have the honor of holding this title"
               :name "Rebounding-Title" }
    :mvp {:desc "The title given to the very best player that season, every MVP has made it to the hall of fame"
               :name "MVP" }
    :finals-mvp {:desc "The title given to the strongest player of the NBA Finals"
               :name "Finals-MVP" }
    :championship-ring {:desc "The most prized possession in the NBA"
               :name "Championship-Ring" }
    :top-draft-pick {:desc "The honor of being first selection among the class of new NBA players"
               :name "Top-Draft-Pick" }
    :all-nba-first-team {:desc "The seven obj"
               :name "All-NBA-First-Team" }
    :first-seed {:desc "A distinction given to the player who has lead his team to be above all others"
               :name "First-Seed" }
    :orange-jacket {:desc "The most sought-after honor in the sport; once in possession of the Orange-Jacket, one is truly the GOAT"
               :name "Orange-Jacket" }
  })

(def init-adventurer
    {:location :akron
     :inventory #{}
     :eligibility #{:mvp :fmvp :first}
     :seasons 0
     :tick 0
     :seen #{:akron}})

(def init-state {:map init-map :items init-items :adventurer init-adventurer})


(defn printState [state]
  (println "map: ")
    (def rooms [:akron :springfield :cleveland :miami :los-angeles :hall-of-fame :nba-awards-show :nba-finals :staples-center])
    (doseq [i (range (count rooms))]
      (println "  " (nth rooms i) "  " (get-in state [:map (nth rooms i)])))
  (println "adventurer: ")
    (def adv-attributes [:location :inventory :key3 :key3 :tick :seen])
    (doseq [i (range (count adv-attributes))]
      (println "  " (nth adv-attributes i) "  " (get-in state [:adventurer (nth adv-attributes i)])))
  (println "items: ")
    (def items [:obj0 :obj1 :obj2 :obj3 :obj4 :obj5 :obj6 :obj7 :obj8])
    (doseq [i (range (count items))]
      (println "  " (nth items i) "  " (get-in state [:items (nth items i)]))))


(defn go [istate dir]
  (let [location (get-in istate [:adventurer :location])
        dest ((get-in istate [:map location :dir]) dir)]
    (if (or (= dest :cleveland) (= dest :miami) (= dest :los-angeles))
        (def state (update-in istate [:adventurer :seasons] #(inc (inc %))))
        (def state istate))
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
    (println (str (get-in state [:items item :name]) ": " (get-in state [:items item :desc])))
    (if-let [val (get-in state [:adventurer :inventory item])]
      (println (str (get-in state [:items item :name]) ": " (get-in state [:items item :desc])))
      (println "Object not in room or inventory")))
  state)

(defn look [state]
  (do
    (print (get-in state [:map (get-in state [:adventurer :location]) :desc]))
    (let [items (vec (get-in state [:map (get-in state [:adventurer :location]) :contents]))]
    (print " Items: ")
      (if (= (count items) 0)
        (print "none. ")
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
          (print (name ((dirs i) 0)) (str "- " (get-in state [:map ((dirs i) 1) :title]) ", "))
          ; (print (name ((dirs i) 0)) (str "- "(name ((dirs i) 1)) ", "))
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

(defn takee [istate item]
  (if (= item :mvp)
    (def tstate (update-in istate [:adventurer :eligibility] #(disj % :mvp)))
  (if (= item :finals-mvp)
    (def tstate (update-in istate [:adventurer :eligibility] #(disj % :fmvp)))
  (if (= item :all-nba-first-team)
    (def tstate (update-in istate [:adventurer :eligibility] #(disj % :first)))
    (def tstate istate))))
  (if (empty? (get-in tstate [:adventurer :eligibility]))
    (do (println "You are now elibigble for the Hall of Fame!")
    (def ttstate (update-in tstate [:adventurer :eligibility] #(conj % :dummy)))
    (def state (update-in ttstate [:map :springfield :dir] #(merge % {:north :hall-of-fame}) ))
    )
    (def state tstate))
  (inventory
  (if-let [val (get-in state [:map (get-in state [:adventurer :location]) :contents item])]
    (let [new-state (update-in state [:adventurer :inventory] #(conj % item))]
      (let [newnew-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(disj % item))]
        newnew-state))
  state)))

(defn dropp [state item]
  (inventory
  (if-let [val (get-in state [:adventurer :inventory item])]
    (let [new-state (update-in state [:adventurer :inventory] #(disj % item))]
      (let [newnew-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(conj % item))]
          newnew-state))
  state)))

(defn actually-transform [state]
  (def new-state (update-in state [:adventurer :inventory] #(conj % :orange-jacket)))
  (def new-new-state (update-in new-state [:adventurer :inventory] #(disj % :mvp :finals-mvp :all-nba-first-team)))
  (println "Your MVP, Finals' MVP, and All-NBA First Team have transformed into an Orange-Jacket!")
  new-new-state
)

(defn transform [state]
    (do
      (if (= :hall-of-fame (get-in state [:adventurer :location]))
          (if-let [val (get-in state [:adventurer :inventory :mvp])]
          (if-let [val (get-in state [:adventurer :inventory :finals-mvp])]
          (if-let [val (get-in state [:adventurer :inventory :all-nba-first-team])]
                (def new-state (actually-transform state))
            (do (print "You dont have the necessary awards with you") (def new-state state)))
            (do (print "You dont have the necessary awards with you") (def new-state state)))
            (do (print "You dont have the necessary awards with you") (def new-state state)))
        (do (print "You can't retire here") (def new-state state)))
        new-state
      )
)

(defn help [state]
  (println "How to play:")
  (println "type 'go north' or 'n' to go north")
  (println "type 'look' to look around")
  (println "type 'take (object name)' to pick up an object")
  (println "type 'drop or place (object name)' to drop an object")
  (println "type 'examine (object name)' to examine an object")
  (println "type 'i' to look at inventory")
  (println "type 'transform' to try to retire")
  (println "type 'help' to see these instructions")
  (println "How to win: \ncollect the MVP, Finals' MVP, and All-NBA First Team. Take these awards to the Hall of Fame and ")
  (println "retire. Then, place your Orange Jacket in your home in Akron and rest peacefully as a GOAT.")


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
          (if (= x "retire") (transform state)
          (let [ret "invalid input"] (println ret) state)))))))))))
     ["go" "@"]
        (fn [x] (go state (keyword x)))
     ["examine" "@"]
        (fn [x] (examine state (keyword x)))
     ["take" "@"]
        (fn [x] (takee state (keyword x)))
     ["drop" "@"]
        (fn [x] (dropp state (keyword x)))
     ["place" "@"]
        (fn [x] (dropp state (keyword x)))])


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
  (if-let [val (get-in local-state [:map :akron :contents :orange-jacket])]
    (println (str "\nYou have achieved GOAT status! You finished in " (get-in local-state [:adventurer :tick]) " moves and " (get-in local-state [:adventurer :seasons]) " seasons."))

    (do (print (str "\nYou are at " (get-in local-state [:map (get-in local-state [:adventurer :location]) :title]) ". "))
    (let [
          _  (println "What do you want to do?")
          command (read-line)]
      (if (= command "quit") (println "bye!")
        (let [new-state (react local-state command)]
        (recur new-state))))))))


(defn -main []
  (println "Hello LeBron James! You have potential to be one of the greatest basketball players of all time - the ")
  (println "potential to be a GOAT. You can travel through time and space to collect various achievments throughout ")
  (println "your career (some useful, some not). In order to achieve GOAT status, you must collect the MVP, Finals' ")
  (println "MVP, and All-NBA First Team. Take these awards to the Hall of Fame and retire; then, place your Orange")
  (println "Jacket in your home in Akron and rest peacefully as a GOAT.\n")
  (help {})
  (repl))
