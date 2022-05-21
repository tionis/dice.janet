#!/bin/env janet
#(math/seedrandom (os/cryptorand 8))
(var rng nil)

(defn roll-one-y-sided-die [y]
  (if (not rng) (set rng (math/rng (os/cryptorand 8))))
  (+ 1 (math/rng-int rng y)))

(defn roll-x-y-sided-dice [x y] 
  (if (= x 0)
      @[]
      (array/concat @[(roll-one-y-sided-die y)] (roll-x-y-sided-dice (- x 1) y))))

(defn roll-x-y-sided-dice-legacy [x y] 
  (var ret @[])
  (loop [i :range [0 x]] (array/push ret (roll-one-y-sided-die y))) ret)

(defn arr-contains? [arr x]
  (label result
    (each item arr
      (if (= item x)
          (return result true)))
    (return result false)))

(defn arr-contains-number [arr]
  (label result
    (each item arr
      (if (number? item)
        (return result true)))
    (return result false)))

(defn get-smallest-number [arr]
  (var smallest 0)
  (each item arr (if (number? item) (set smallest item)))
  (each item arr
    (if (number? item)
        (if (< item smallest)
            (set smallest item))))
  smallest)

(defn get_success_number [sides] (- sides (math/ceil (/ sides 3.5)))) # TODO integrate with parse-modifiers
(defn get_fail_number [sides] (math/floor (/ sides 6))) # TODO integrate with parse-modifiers

(defn parse-modifiers [modifiers]
  (var ret @{})
  (put ret :again 10)
  (put ret :success 8)
  (put ret :fail 1)
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
  (table/to-struct ret))

(defn should_reroll_general? [number modifiers] (>= number (modifiers :again)))
(defn should_reroll_first_roll? [number modifiers] (if (should_reroll_general? number modifiers) true (modifiers :rote)))
(defn is_success? [number modifiers] (>= number (modifiers :success)))

(defn format_result [result]
  (def max_i (- (length result) 1))
  (loop [i :range [0 (length result)]]
    (prin "[")
    (def max_j (- (length (result i))))
    (loop [j :range [0 (length (result i))]]
      (if (> j 0)
          (prin (string " -> " ((result i) j)))
          (prin ((result i) j))))
    (if (= i max_i) (prin "]") (prin "] ")))
  (print))

(defn count_successes [result modifiers]
  (var successes 0)
  (each line result
    (each item line
      (if (>= item (modifiers :success)) (++ successes))))
  successes)

(defn count_fails [result modifiers]
  (var fails 0)
  (each line result
    (if (<= (line 0) (modifiers :fail)) (++ fails)))
  fails)

(defn get_result_message [amount modifiers result]
  (def successes (count_successes result modifiers))
  (def fails (count_fails result modifiers))
  (if (> fails (math/floor (/ amount 2)))
      (if (= successes 0)
          (print "Crit Fail")
          (print "Fail with " successes " successes"))
      (if (>= successes 5)
          (print "Crit Success with " successes " successes")
          (if (= successes 0)
            (print "Fail with 0 successes")
            (print "Success with " successes " successes")))))

(defn cod_roll [amount & modifiers]
  (if (= amount nil) (error "Not a number!"))
  (def modifiers (parse-modifiers modifiers))
  (def result @[])
  (loop [i :range [0 amount]]
    (array/push result @[])
    (var continue true)
    (var recursion 0)
    (while continue
      (++ recursion)
      (def die_result (roll-one-y-sided-die 10))
      (if (and (< die_result (modifiers :again))
               (or (not (modifiers :rote)) (and (modifiers :rote)
                                                (> recursion 1))))
          (set continue false))
      (array/push (get result i) die_result)))
  (format_result result)
  (get_result_message amount modifiers result))

(defn roll_chance [& modifiers]
  (def result (roll-one-y-sided-die 10))
  (cond
    (= result 1)  (print "Crit Fail!")
    (= result 10) (print "Success!")
    (print "Fail!")))

(defn roll_init [& args]
  (def result (roll-one-y-sided-die 10))
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
                    (def result (roll-x-y-sided-dice (scan-number (dice_arr 0)) (scan-number (dice_arr 1))))
                    (prin "Result: ") (pp result)
                    (print "Sum: " (sum result)))
                (let [number (scan-number dice)]
                  (if number
                    (cod_roll number ;args)
                    (case dice
                      "chance" (roll_chance ;args)
                      "init" (roll_init ;args)
                      (error "Unknown command!")))))
    :number (cod_roll dice ;args) # TODO fix this (new handling of modifiers?)
    :keyword (case dice
               :chance (roll_chance ;args)
               :init (roll_init ;args))))

(defn main [_ & args]
  (def args_count (length args))
  (cond
    (= args_count 0) (do (print "Please supply the dice to roll!") (os/exit 1))
    (> args_count 0) (roll ;args)
    (print "unsupported amount of arguments")))
