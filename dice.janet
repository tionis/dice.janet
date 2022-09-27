#!/bin/env janet
#(math/seedrandom (os/cryptorand 8))
(var rng nil)
(defmacro s+ "Appends string to var x" [x & strings] ~(set ,x (string ,x ,;strings)))

(defn roll_one_y_sided_die [y]
  (if (not rng) (set rng (math/rng (os/cryptorand 8))))
  (+ 1 (math/rng-int rng y)))

(defn roll_x_y_sided_dice [x y] 
  (if (= x 0)
      @[]
      (array/concat @[(roll_one_y_sided_die y)] (roll_x_y_sided_dice (- x 1) y))))

(defn roll_x_y_sided_dice-legacy [x y] 
  (var ret @[])
  (loop [i :range [0 x]] (array/push ret (roll_one_y_sided_die y))) ret)

(defn- arr-contains? [arr x]
  (label result
    (each item arr
      (if (= item x)
          (return result true)))
    (return result false)))

(defn- arr-contains-number [arr]
  (label result
    (each item arr
      (if (number? item)
        (return result true)))
    (return result false)))

(defn- get-smallest-number [arr]
  (var smallest 0)
  (each item arr (if (number? item) (set smallest item)))
  (each item arr
    (if (number? item)
        (if (< item smallest)
            (set smallest item))))
  smallest)

(defn- get_success_number [sides] (- sides (math/ceil (/ sides 3.5))))
(defn- get_fail_number [sides] (math/floor (/ sides 6)))

(defn- parse-modifiers [sides modifiers]
  (var ret @{})
  (put ret :again sides)
  (put ret :success (get_success_number sides))
  (put ret :fail (get_fail_number sides))
  (put ret :rote false)
  (each item modifiers
    (case (type item)
      :string (do (def number (scan-number item))
                  (if number
                      (if (< number (ret :again))
                          (do (put ret :again number)
                              (if (< number (ret :success)) (put ret :success number))))
                      (case item
                        "r" (put ret :rote true)
                        "n" (do (put ret :again 11)
                                (put ret :rote false)))))
      :keyword (case item
                  :rote (put ret :rote true)
                  :no_reroll (put ret :again 11)
                  :r (put ret :rote true)
                  :n (put ret :again 11))
      :number (if (< item (ret :again))
                  (do (put ret :again item)
                      (if (< item (ret :success)) (put ret :success item))))
      (error "Could not parse!")))
  (if (< (ret :again) 2)
      (error "Again modifier to low!"))
  (table/to-struct ret))

(defn- should_reroll_general? [number modifiers] (>= number (modifiers :again)))
(defn- should_reroll_first_roll? [number modifiers] (if (should_reroll_general? number modifiers) true (modifiers :rote)))
(defn- is_success? [number modifiers] (>= number (modifiers :success)))

(defn- format_result [result]
  (var ret "")
  (loop [i :range [0 (length result)]]
  (def max_i (- (length result) 1))
    (s+ ret "[")
    (def max_j (- (length (result i))))
    (loop [j :range [0 (length (result i))]]
      (if (> j 0)
          (s+ ret (string " -> " ((result i) j)))
          (s+ ret ((result i) j))))
    (if (= i max_i) (s+ ret "]") (s+ ret "] ")))
  ret)

(defn- count_successes [result modifiers]
  (var successes 0)
  (each line result
    (each item line
      (if (>= item (modifiers :success)) (++ successes))))
  successes)

(defn- count_fails [result modifiers]
  (var fails 0)
  (each line result
    (if (<= (line 0) (modifiers :fail)) (++ fails)))
  fails)

(defn- get_result_message [amount modifiers result]
  (var ret "")
  (def successes (count_successes result modifiers))
  (def fails (count_fails result modifiers))
  (if (> fails (math/floor (/ amount 2)))
      (if (= successes 0)
          (s+ ret "Crit Fail")
          (s+ ret "Fail with " successes " successes"))
      (if (>= successes 5)
          (s+ ret "Crit Success with " successes " successes")
          (if (= successes 0)
            (s+ ret "Fail with 0 successes")
            (s+ ret "Success with " successes " successes"))))
  ret)

(defn- get_status [amount modifiers result]
  (def successes (count_successes result modifiers))
  (def fails (count_fails result modifiers))
  (if (> fails (math/floor (/ amount 2)))
      (if (= successes 0)
          :crit_fail
          :fail)
      (if (>= successes 5)
          :crit_success
          (if (= successes 0)
              :fail
              :success))))

(defn mass_init [init_table]
  (def result @[])
  (def init_mods @{})
  (each char init_table
    (put init_mods (char 0) (char 1))
    (def die_result (roll_one_y_sided_die 10))
    (var init_result (+ die_result (char 1)))
    (each modifier (slice char 2 -1)
      (case modifier
        :advantage (do (def die_result_2 (roll_one_y_sided_die 10))
                       (def init_result_2 (+ die_result_2 (char 1)))
                       (if (> init_result_2 init_result)
                           (set init_result init_result_2)))
        (error (string "modifier not implemented: " modifier))))
    (array/push result @[(char 0) init_result]))
  (sort result (fn [x y] (if (= (x 1) (y 1))
                               (> (init_mods (x 0)) (init_mods (y 0)))
                               (> (x 1) (y 1))))))

(defn cod_roll [sides amount & modifiers]
  (if (= amount nil) (error "Not a number!"))
  (def modifiers (parse-modifiers sides modifiers))
  (def result @[])
  (loop [i :range [0 amount]]
    (array/push result @[])
    (var continue true)
    (var recursion 0)
    (while continue
      (++ recursion)
      (def die_result (roll_one_y_sided_die sides))
      (if (and (< die_result (modifiers :again))
               (or (not (modifiers :rote)) (and (modifiers :rote)
                                                (> recursion 1))))
          (set continue false))
      (array/push (get result i) die_result)))
  (print (format_result result))
  (print (get_result_message amount modifiers result))
  {:result result
   :status (get_status amount modifiers result)
   :successes (count_successes result modifiers)})

(defn- roll_chance [& modifiers]
  (def result (roll_one_y_sided_die 10))
  (print "[" result "]")
  (cond
    (= result 1)  (print "Crit Fail!")
    (= result 10) (print "Success!")
    (print "Fail!")))

(defn- roll_init [& args]
  (def result (roll_one_y_sided_die 10))
  (if (= (length args) 0)
      (print "Your result: [" result "]")
      (do (def number (scan-number (args 0)))
          (if number
              (print "Your result: " result " + " number " = [" (+ result number) "]")
              (error "Could not parse init mod")))))

(defn roll [dice & args]
  (case (type dice)
    :string (if (peg/match ~(* (some :d) "d" (some :d)) dice)
                (do (def dice_arr (string/split "d" dice))
                    (def result (roll_x_y_sided_dice (scan-number (dice_arr 0)) (scan-number (dice_arr 1))))
                    (prin "Result: ") (pp result)
                    (print "Sum: " (sum result)))
                (let [number (scan-number dice)]
                  (if number
                    (cod_roll 10 number ;args)
                    (case dice
                      "chance" (roll_chance ;args)
                      "init" (roll_init ;args)
                      (error "Unknown command!")))))
    :number (cod_roll 10 dice ;args)
    :keyword (case dice
               :chance (roll_chance ;args)
               :init (roll_init ;args))))

(defn multi [amount init &opt name]
  (default name "npc")
  (def ret @[])
  (loop [i :range [1 (+ amount 1)]]
    (array/push ret [(string name "-" i) init]))
  ret)

(defn main [_ & args]
  (def args_count (length args))
  (cond
    (= args_count 0) (do (print "Please supply the dice to roll!") (os/exit 1))
    (> args_count 0) (roll ;args)
    (print "unsupported amount of arguments")))
