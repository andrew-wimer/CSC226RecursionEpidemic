;;ANDREW WIMER
;;CSC 226 APRIL 2020
;;PROJECT 3 PROGRAM 3: ELEMENTARY LEVEL EPIDEMIC MODELING
;;very program for modeling epidemics, intended to show use of recursion, subprograms, and subprogram
;;parameters in Clojure

;;below is our namespace
(ns project3demo1.core
  (:gen-class))
(require '[clojure.edn :as edn])                            ;;library required to read inputted data into edn format
;;this allows us to use functions that convert string input to other data types more easily


;;;====================PROMPTS FOR VARIABLES
;; These will be our prompts for variable entry. All variables defined here will have a global scope,
;;despite being inside of a function's scope. Ordinarily we would not want to do this, but
;;we'll go with it here for the ease of demonstration.
;;->When these functions are called, the user is prompted to enter the specified number.
;;The variable will now be defined as number the user inputs.
;;This function won't return that input, but instead will return that input converted from a string
;;to a number (integer or float depending upon number entered).
(defn promptR0 []                                           ;;function definition
  (println "Enter person-to-person infection rate aka r0): ") ;;input prompt
  (def r0 (read-line))                                      ;;defining variable
  (float (edn/read-string r0)))                             ;;float conversion of input

(defn promptDays []
  (println "Enter the number of days since the spread of infection started: ")
  (def numDays (read-line))
  (edn/read-string numDays))                                ;;ordinary conversion of input to an integer

(defn promptInitInfected []
  (println "Enter the initial number of infected: ")
  (def numInitInfected (read-line))
  (edn/read-string numInitInfected))

(defn promptDeceased []
  (println "Enter the number of deceased: ")
  (def numDeceased (read-line))
  (edn/read-string numDeceased))

(defn promptInfected []
  (println "Enter the number of infected: ")
  (def numInfected (read-line))
  (edn/read-string numInfected))

(defn promptIFR []
  (println "Enter the infection fatality rate (IFR): ")
  (def numIFR (read-line))
  (float (edn/read-string numIFR)))

;;====================CALCULATION FUNCTIONS
;;These are our functions for making calculations that will be needed elsewhere.

;;calcCFR
;calculates Infection Fatality Rate using numbers of deceased and infected as parameters.
;;returns the float form of deceased divided by total infected
(defn calcIFR [deceased infected]
  (float (/ deceased infected)))

;;calcDeceased
;;Calculates number deceased using the number of infected and IFR as parameters
;;Returns number of infected, multiplied by the IFR (itself divided by 100)
(defn calcDeceased [infections ifr]
  (* infections  (/ ifr 100)))

;;calcTotalNewInfections
;;Calculates, on a given date from the start of an epidemic, what that days' new infection numbers will be.
;;Accepts number of infected, the r0, and days since start as parameters.
(defn calcTotalNewInfections [numInfected r0 days]
  (if (= days 1) ;;If there is only 1 day, there can't be any new infections so number infected stays the same.
    numInfected                                             ;;The function will recursively decrement to this branch
    ;;regardless, and that number of infected will be the value returned by the function.
    (* (calcTotalNewInfections numInfected r0 (dec days)) r0))) ;;but if there is more than one day...
;;the function will recursively calculate the number of new infected day by day until
;;the given day count decrements down to 1.


;;calcTotalInfections
;;Calculates how many will be infected by a given day of an epidemic.
;;Takes prior number of new cases and prior number of total infected, as well as r0 and days since epidemic start
;;as parameters.
(defn calcTotalInfections [priorNew priorTotal r0 days]
  (if (= days 1)
    priorTotal                                            ;;branch 1:  either already at or
    ;; decremented down to day 1, this is the result the function will return
    (calcTotalInfections                                  ;; branch 2: recursive calculation of total infections
      (* priorNew r0)                                    ;; first the prior number of new cases is updated by
      ;;multiplying it by the infection rate to get the new "prior new cases"
      (+ (* priorNew r0) priorTotal)                        ;;then the updated number of prior cases is added to
      ;;the prior total number of cases
      r0
      (dec days))))

;;=====================MODES OF OPERATION

;;ifrMode
;; Infection fatality rate mode
;;Uses calcIFR function with input values for deceased and infected to find the infection fatality rate.
;; Postcondition: prints the calculated infection fatality rate converted to a percentage.
(defn ifrMode []
  (println "IFR Mode")
(println "Infection fatality rate: %" (format "%.2f" (* 100 (calcIFR (promptDeceased)
           (promptInfected))))))
;;call to calcIFR uses functions as parameters

;;totalNewInfectMode
;;prints number of new infected on a given day, calculated from user inputted numbers for initial infected, r0,
;; and days  since start of epidemic.
(defn totalNewInfectMode []
  (println "Total New Infections on a Given Day from the Start")
  (println "Total new infections on given day: "
           (calcTotalNewInfections (promptInitInfected) (promptR0) (promptDays))))

;;totalInfectMode
;;Prints number of total infected, calculated from user inputted numbers for initial infected, r0
;; and days since start of epidemic.
(defn totalInfectMode []
  (println "Total Population Infected By a Given Day")
  (println "Total people infected by given day: "
           (calcTotalInfections (promptInitInfected) (edn/read-string numInitInfected) (promptR0) (promptDays))))
;;the second parameter is the numeric conversion of "numInitInfected", a variable defined in promptInitInfected
;;with a global scope.


;;deceasedMode
;;prints number of deceased from user inputted number of infected and IFR
(defn deceasedMode []
  (println "How many deceased with a given IFR")
 (println "Deceased: " (calcDeceased (promptInfected) (promptIFR))))


;;;===================== MENU FUNCTIONS

;;Our menu options will be stored as a list of strings
(def mainMenuOptionList '("1) Calculate infected fatality rate."
                           "2) Calculate total new population infected on a given day."
                           "3) Calculate total population infected by a given day."
                           "4) Calculate population deceased. "))

;;displayMainMenu
;;Displays the main menu
(defn displayMainMenu []
  (println "Here are your options: ")
  (doseq [option mainMenuOptionList]                        ;;list of options is treated as a sequence
    ;;and the list will be printed sequentially in order of first input to last input
    (println option)))

;;menuInput
;;
(defn menuInput []
  (println "Enter your choice: ")
  (def y (read-line))
  (let [x (edn/read-string y)]                              ;;"let" function: think logic
    ;; and mathematical proofs. "let x be the value of y".
    ;; x will only be assigned this value in the scope of
    ;;the "let" function.
    ;; "cond" function allows for a series of conditionals
    ;; all dependent on what we "let" x be.
    ;; here, the value of x will determine which mode of the program we will use.
    (cond
      (= x 1) (ifrMode)
      (= x 2) (totalNewInfectMode)
      (= x 3) (totalInfectMode)
      (= x 4) (deceasedMode)
      :else (System/exit 0))))                          ;;every conditional sequence ends with an :else
;;here, the :else is a system exit to end the program

;;Run program
;;Begins the flow of program control
(defn runProgram []
  (displayMainMenu)
  (menuInput))

;; MAIN FUNCTION
;; this goes at the bottom of our core namespace and serves as the entry to the program proper
(defn -main
  [& args]
  (runProgram))

;;end of the namespace