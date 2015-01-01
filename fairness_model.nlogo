turtles-own [poidsVOffer p11 q11 p22 q22 p12 p21 q12 q21 pq gamma rhos rhop mu lambdas lambdap poidsWOffer numberOfOffsprings timesProp timesResp poidsVRequest poidsWRequest strength payoff cumulatedPayoff paired partner interactionLength myEffort]
globals [ generation linkageDesequilibrium rejectedOffers12 rejectedOffers11 rejectedOffers22 interactionsLength lowerLimitWeight rejectedOffers21 tempOutputList globalList globalListNames outputListNames outputList outputFile seed printProd printVectorVRequest printVectorWRequest printVectorVOffer printVectorWOffer payoffResponder21 payoffResponder12  payoffResponder11 payoffResponder22 timeElapsed strengthMax upperLimitWeight numberInputsOffers numberInputsRequests]
breed [offspring offsprings]
breed [parent parents]

;; Netlogo simulation for the evolution of fairness by partner choice, with asymmetries of strength between partners
;; Model explained and results published in "Evolution of equal division among unequal partners", Evolution, 2014
;; If details are needed on how this model works, please contact the first author of the publication (find contact on personal website at http://stephanedebove.net).
;; 2014
;; Stéphane Debove
;; Gnu GPL license
;; v 1.0

to setup
  clear-all
  set seed new-seed
  random-seed seed  
  ;set seed 4
  set generation 0 set timeElapsed 0 set strengthMax 2 
  ifelse geneticImplementation = "neuralNetwork" [set upperLimitWeight 10 set lowerLimitWeight -10][set upperLimitWeight 1 set lowerLimitWeight 0]
  set payoffResponder21 [0] set payoffResponder12 [0] set rejectedOffers21 [0] set rejectedOffers12 [0]
  set payoffResponder22 [0] set payoffResponder11 [0] set rejectedOffers11 [0] set rejectedOffers22 [0]  
  set outputList [] set tempOutputList [] set interactionsLength [0]
  
  set numberInputsOffers 2 
  set numberInputsRequests 2 ;; and number of inputs for the neural network for making offers and requests
  create-turtles populationSize [ setxy random-xcor random-ycor ]
  ask turtles [
    ;;set poidsVOffer n-values numberOfHiddenNeurons [n-values numberInputsOffers [(random-float (upperLimitWeight * 2)) + lowerLimitWeight]]
    ;;set poidsWOffer n-values numberOfHiddenNeurons [(random-float (upperLimitWeight * 2)) + lowerLimitWeight]
    ;;set poidsVRequest n-values numberOfHiddenNeurons [n-values numberInputsRequests [(random-float (upperLimitWeight * 2)) + lowerLimitWeight]]
    ;;set poidsWRequest n-values numberOfHiddenNeurons [(random-float (upperLimitWeight * 2)) + lowerLimitWeight]    
    set gamma 0 set rhos -1 set rhop 1 set mu 0 set lambdas -1 set lambdap -1
    set poidsVOffer n-values numberOfHiddenNeurons [n-values numberInputsOffers [0]]
    set poidsWOffer n-values numberOfHiddenNeurons [0]
    set poidsVRequest n-values numberOfHiddenNeurons [n-values numberInputsRequests [0]]
    set poidsWRequest n-values numberOfHiddenNeurons [0]  
    set shape "person"
    set p11 pStart set q11 qStart set p22 pStart set q22 qStart
    set p11 pStart set q11 qStart set p22 pStart set q22 qStart    
    set breed parent
  ]
  set globalListNames (list "seed" "beta" "tau" "continuum" "probabilityUniformMutation" "populationSize" "lifespan" "mutationRate" "numberOfGenerations" "mutationStd" "timerHours")    
  set globalList (list seed beta tau continuum probabilityUniformMutation populationSize lifespan mutationRate numberOfGenerations mutationStd)  
  set outputListNames (list "Generation" "mean payoffResponder22" "mean payoffResponder11" "mean payoffResponder21" "mean payoffResponder12" "length rejectedOffers22" "length rejectedOffers11" "length rejectedOffers21" "length rejectedOffers12" "gamma" "rhos" "rhop" "mu" "lambdas" "lambdap")
  set outputFile (word "output_" seed ".txt")
  reset-timer
  reset-ticks
 
end

to resetValues
  set printProd -1 set timeElapsed 0
  set payoffResponder21 [] set payoffResponder12 [] set rejectedOffers12 [] set rejectedOffers21 [] set interactionsLength []
  set payoffResponder22 [] set payoffResponder11 [] set rejectedOffers22 [] set rejectedOffers11 []
  ask turtles [
    ifelse continuum = "No"
    [
      ifelse random-float 1 < probaWeak [set strength strength1][set strength strength2]  
    ]
    [
       set strength (random-float (strength2 - strength1)) + strength1
    ]
    
    set paired 0 ;;boolean, paired or not
    set partner -1 ;; number of partner
    set payoff 0 ;; payoff in the current time lapse, will be > 0 only if paired
    set cumulatedPayoff 0 ;; payoff since the beginning of life
    set interactionLength -1 ;; for the situation with effort, time remaining before going back to solitary state
    set timesProp 0
    set timesResp 0
    set numberOfOffsprings 1
    set breed parent
  ]
end


to go
  if ticks = 0 [reset-timer]
  if ticks = (numberOfGenerations + 1) 
  [ 
    printToFile
  
    ;;print timer
    stop 
    
  ]
  resetValues 
  liveYourLife 
  if (ticks mod updatePlotsEvery = 0) [
    
    update-plots    
    set-current-plot "number of times proposer"
    clear-plot
    foreach sort turtles [ask ? [plot timesProp]]
    set-current-plot "number of times responder"
    clear-plot
    foreach sort turtles [ask ? [plot timesResp]]   
    let mean11 0 let mean22 0 let mean12 0 let mean21 0
    let mean11_2 0 let mean22_2 0 let mean12_2 0 let mean21_2 0
    ifelse empty? payoffResponder11 [set mean11 "Na"][set mean11 mean payoffResponder11]
    ifelse empty? payoffResponder22 [set mean22 "Na"][set mean22 mean payoffResponder22]
    ifelse empty? payoffResponder12 [set mean12 "Na"][set mean12 mean payoffResponder12]  
    ifelse empty? payoffResponder21 [set mean21 "Na"][set mean21 mean payoffResponder21]  
    ifelse empty? rejectedOffers11 [set mean11_2 "Na"][set mean11_2 length rejectedOffers11]
    ifelse empty? rejectedOffers22 [set mean22_2 "Na"][set mean22_2 length rejectedOffers22]
    ifelse empty? rejectedOffers12 [set mean12_2 "Na"][set mean12_2 length rejectedOffers12]  
    ifelse empty? rejectedOffers21 [set mean21_2 "Na"][set mean21_2 length rejectedOffers21]     
    set tempOutputList (list ticks mean22 mean11 mean21 mean12 mean22_2 mean11_2 mean21_2 mean12_2 mean [gamma] of turtles mean [rhos] of turtles mean [rhop] of turtles mean [mu] of turtles mean [lambdas] of turtles mean [lambdap] of turtles)
    set outputList lput tempOutputList outputList

  ]
  reproduceAndDie
  
  tick-advance 1
end

to liveYourLife
  while [ timeElapsed < lifespan ] ;; while we haven't reached the end of the life of all individuals
  [
    let numberOfPairedIndividuals count turtles with [paired = 1]
    let numberOfSolitaryIndividuals count turtles with [paired = 0]
    ;;let betaEff beta * (numberOfSolitaryIndividuals - 1) / (populationSize - 1)
    let betaEff beta
    let lambdaExpo (numberOfPairedIndividuals * tau / 2) + (numberOfSolitaryIndividuals) * betaEff ;; the lambda parameter of the exponential distribution
    let timeUntilNextEvent random-exponential (1 / lambdaExpo) 
    ask turtles [ set cumulatedPayoff (cumulatedPayoff + payoff * timeUntilNextEvent) ] ;; give payment for the previous timelapse    
    
    let threshold (numberOfSolitaryIndividuals * betaEff) / lambdaExpo ;; threshold determining if the event will be encounter or split
    let randomNumber random-float 1
    ifelse (randomNumber <= threshold) and (randomNumber != 0)
    [ ;; encounter
      let proposer -1
      let responder -1
      let turtle1 one-of turtles with [paired = 0] 
      ask turtle1 [ set paired 1 ] ;; to avoid picking the same guy for second individual
      let turtle2 one-of turtles with [paired = 0]
      ifelse [strength] of turtle1 = [strength] of turtle2 ;; strength means strength here!!!
      [
        ifelse random-float 1 < 0.5
        [
          set proposer turtle1
          set responder turtle2
        ]
        [
          set proposer turtle2
          set responder turtle1
        ]
      ]
      [
        ifelse [strength] of turtle1 > [strength] of turtle2
        [ set proposer turtle1 set responder turtle2 ]
        [ set proposer turtle2 set responder turtle1 ]
      ]
      let offer -1
      let request -1
      let pie 1
      
      ifelse geneticImplementation = "neuralNetwork" 
      [
        let listInputOffers (list [strength] of proposer [strength] of responder) ;; list of inputs for the neural network 
        set offer neuralNetwork ([poidsVOffer] of proposer) ([poidsWOffer] of proposer) listInputOffers
        let listInputRequests []
        set listInputRequests (list ([strength] of proposer) ([strength] of responder))
        set request neuralNetwork ([poidsVRequest] of responder) ([poidsWRequest] of responder) listInputRequests
      ]
      [
        ifelse geneticImplementation = "pq"
        [
          if [strength] of responder = [strength] of proposer and [strength] of responder = strength1 [set offer [p11] of proposer]
          if [strength] of responder = [strength] of proposer and [strength] of responder = strength2 [set offer [p22] of proposer]          
          if [strength] of responder > [strength] of proposer [set offer [p12] of proposer]       
          if [strength] of responder < [strength] of proposer [set offer [p21] of proposer]     
          if [strength] of responder = [strength] of proposer and [strength] of responder = strength1 [set request [q11] of responder]
          if [strength] of responder = [strength] of proposer and [strength] of responder = strength2 [set request [q22] of responder]          
          if [strength] of responder > [strength] of proposer [set request [q12] of responder]       
          if [strength] of responder < [strength] of proposer [set request [q21] of responder] 
        ]
        [
          ;set offer [gamma] of proposer * (1 - [rho] of proposer + [rho] of proposer * (abs ([strength] of responder - [strength] of proposer)))
          ;set request [mu] of responder * (1 - [lambda] of responder + [lambda] of responder * (abs ([strength] of responder - [strength] of proposer)))
          set offer [gamma] of proposer  + [rhos] of proposer * [strength] of proposer + [rhop] of proposer * [strength] of responder
          set request [mu] of responder + [lambdas] of responder * [strength] of responder + [lambdap] of responder * [strength] of proposer
          if offer < 0 [set offer 0]
          if offer > 1 [set offer 1]
          if request < 0 [set request 0]
          if request > 1 [set request 1]
        ]
      ]
      
      
      
      ;ifelse random-float 1 < request
      ifelse offer >= request
      [ ;; offer accepted
        
        ask responder [ 
          set paired 1
          set payoff offer * pie
          set partner [who] of proposer
          set timesResp timesResp + 1
          set interactionLength timeElapsed
        ] 
        ask proposer [ 
          set paired 1
          set payoff pie - offer * pie
          set partner [who] of responder
          set timesProp timesProp + 1
          set interactionLength timeElapsed
        ]        
        if (ticks mod updatePlotsEvery = 0) [    
          if ([strength] of responder = strength1) and ([strength] of proposer = strength1) [set payoffResponder11 lput [payoff] of responder payoffResponder11]
          if ([strength] of responder = strength2) and ([strength] of proposer = strength2) [set payoffResponder22 lput [payoff] of responder payoffResponder22]
          if ([strength] of responder > [strength] of proposer) [set payoffResponder12 lput [payoff] of responder payoffResponder12]          
          if ([strength] of responder < [strength] of proposer) [set payoffResponder21 lput [payoff] of responder payoffResponder21]
          ;ifelse [strength] of responder = strength1 [set payoffResponder11 lput [payoff] of responder payoffResponder11][set payoffResponder22 lput [payoff] of responder payoffResponder22]
        ] 
      ]
      [ ;; offer rejected
        if (ticks mod updatePlotsEvery = 0) [
          if ([strength] of responder = strength1) and ([strength] of proposer = strength1) [set rejectedOffers11 lput (offer * pie) rejectedOffers11]
          if ([strength] of responder = strength2) and ([strength] of proposer = strength2) [set rejectedOffers22 lput (offer * pie) rejectedOffers22]
          if ([strength] of responder > [strength] of proposer) [set rejectedOffers12 lput (offer * pie) rejectedOffers12]          
          if ([strength] of responder < [strength] of proposer) [set rejectedOffers21 lput (offer * pie) rejectedOffers21]       
          ;ifelse [strength] of responder = strength1 [set rejectedOffers11 lput [payoff] of responder rejectedOffers11][set rejectedOffers22 lput [payoff] of responder rejectedOffers22]          
        ]
        ask turtle1 [ set paired 0]
        ask proposer [ set interactionLength -1]
        ask responder [ set interactionLength -1]
      ] 
    ] 
    
    [ ;; split
      let split1 one-of turtles with [paired = 1]
      let split2 turtle [partner] of split1
      set interactionsLength lput (timeElapsed - [interactionLength] of split1) interactionsLength ;; to know how long the interaction lasted
      ask split1 [
        set interactionLength -1
        set paired 0
        set payoff 0
        set partner -1
      ]
      ask split2 [
        set interactionLength -1
        set paired 0 
        set payoff 0
        set partner -1
      ]      
    ]
    set timeElapsed timeElapsed + timeUntilNextEvent    ;;increment time 
  ]
end

to reproduceAndDie
  
  let averagePayoff mean [cumulatedPayoff] of turtles  
  let offspringTable []
  let offspringTableTurtle []
  if averagePayoff != 0 [

    ask turtles [ set numberOfOffsprings round (1000 * cumulatedPayoff / averagePayoff)]
  ]
  
  ask turtles [
    set offspringTableTurtle n-values numberOfOffsprings [who] ;; create a table containing as many cases as offspring, with the who of the parent inside
    set offspringTable (sentence offspringTable offspringTableTurtle) ;; concatenate to the previous table
  ]
  if (ticks mod updatePlotsEvery = 0) [
    set-current-plot "expected number of offsprings"
    clear-plot
    foreach sort turtles [ask ? [plot numberOfOffsprings]] 
  ]  
  let newGeneration n-values populationSize [one-of offspringTable]   ;; random drawing with replacement
  
  (foreach newGeneration
    [
      ask turtle ? [hatch-offspring 1] ;; parents make offsprings in the breed offspring
    ]) 
  ask turtles with [breed = parent] [die]
  
  if recombination = "Yes" 
  [
    let randomTurtle one-of turtles
    ifelse geneticImplementation = "neuralNetwork"
    [
      ask turtles [
        ifelse random-float 1 < 0.5 ;; we swap the neural networks for offers
        [
          set randomTurtle one-of turtles
          let temp1 poidsVOffer
          let temp2 poidsWOffer
          set poidsVOffer [poidsVOffer] of randomTurtle
          set poidsWOffer [poidsWOffer] of randomTurtle
          ask randomTurtle [ set poidsVOffer temp1 set poidsWOffer temp2 ]
        ]
        [ ;; we swap the neural networks for requests
          set randomTurtle one-of turtles
          let temp1 poidsVRequest
          let temp2 poidsWRequest
          set poidsVRequest [poidsVRequest] of randomTurtle
          set poidsWRequest [poidsWRequest] of randomTurtle
          ask randomTurtle [ set poidsVRequest temp1 set poidsWRequest temp2 ]
        ]
      ]
    ]
    [
      ifelse geneticImplementation = "pq"
      [
        
        ask turtles [
          let randomNumber random-float 1
          set randomTurtle one-of turtles
          let temp -100
          if randomNumber < 0.125 [set temp p11 set p11 [p11] of randomTurtle ask randomTurtle [ set p11 temp ]]
          if randomNumber >= 0.125 and randomNumber < 0.25 [set temp p22 set p22 [p22] of randomTurtle ask randomTurtle [ set p22 temp ]]
          if randomNumber >= 0.25 and randomNumber < 0.375 [set temp p12 set p12 [p12] of randomTurtle ask randomTurtle [ set p12 temp ]]
          if randomNumber >= 0.375 and randomNumber < 0.5 [set temp p21 set p21 [p21] of randomTurtle ask randomTurtle [ set p21 temp ]]          
          if randomNumber >= 0.5 and randomNumber < 0.625 [set temp q22 set q22 [q22] of randomTurtle ask randomTurtle [ set q22 temp ]]          
          if randomNumber >= 0.625 and randomNumber < 0.75 [set temp q11 set q11 [q11] of randomTurtle ask randomTurtle [ set q11 temp ]]                    
          if randomNumber >= 0.75 and randomNumber < 0.875 [set temp q12 set q12 [q12] of randomTurtle ask randomTurtle [ set q12 temp ]]                    
          if randomNumber >= 0.875 [set temp q21 set q21 [q21] of randomTurtle ask randomTurtle [ set q21 temp ]]         
        ]
      ]
      [
        ask turtles [
          ifelse random-float 1 < 0.5 ;; we swap the function for offers
          [
            set randomTurtle one-of turtles
            let temp1 gamma
            let temp2 rhos
            let temp3 rhop
            set gamma [gamma] of randomTurtle
            set rhos [rhos] of randomTurtle
            set rhop [rhop] of randomTurtle
            ask randomTurtle [ set gamma temp1 set rhos temp2 set rhop temp3]
          ]
          [ ;; we swap the functions for requests
            set randomTurtle one-of turtles
            let temp1 mu
            let temp2 lambdas
            let temp3 lambdap
            set mu [mu] of randomTurtle
            set lambdas [lambdas] of randomTurtle
            set lambdap [lambdap] of randomTurtle
            ask randomTurtle [ set mu temp1 set lambdas temp2 set lambdap temp3 ]
          ]
        ]
      ]        
    ]
    
  ]
  
  ifelse geneticImplementation = "neuralNetwork"
  [
    ask turtles [ 
      setxy random-xcor random-ycor ;; to be able to right click on a turtle
      set poidsWOffer map applyMutation poidsWOffer
      set poidsWRequest map applyMutation poidsWRequest
      let minilist [] 
      let indexes n-values numberOfHiddenNeurons [ ? ]
      (foreach poidsVOffer indexes ;; more complicated for poidsVOffer, which is a vector of vectors
        [
          set minilist ?1
          set minilist map applyMutation minilist
          set poidsVOffer replace-item ?2 poidsVOffer minilist
        ])         
      (foreach poidsVRequest indexes ;; idem for poidsVRequest
        [
          set minilist ?1
          set minilist map applyMutation minilist
          set poidsVRequest replace-item ?2 poidsVRequest minilist
        ]) 
    ]    
    
  ]
  [
    ifelse geneticImplementation = "pq"
    [
      ask turtles [ 
        setxy random-xcor random-ycor ;; to be able to right click on a turtle
        set p11 applyMutation p11
        set q11 applyMutation q11
        set p22 applyMutation p22
        set q22 applyMutation q22      
        set p12 applyMutation p12
        set q12 applyMutation q12
        set p21 applyMutation p21
        set q21 applyMutation q21   
      ]
    ] 
    [
      ask turtles [ 
        setxy random-xcor random-ycor ;; to be able to right click on a turtle
        set mu applyMutation mu
        set gamma applyMutation gamma          
      ]
      let oldLowerLimitWeight lowerLimitWeight ;; lambda and rho vary between -1 and 1, but mu and gamma between 0 and 1, so we need to change the lower limit
      set lowerLimitWeight -1
      ask turtles [ 
        setxy random-xcor random-ycor ;; to be able to right click on a turtle
        set lambdas applyMutation lambdas
        set lambdap applyMutation lambdap
        set rhos applyMutation rhos
        set rhop applyMutation rhop        
      ]
      set lowerLimitWeight oldLowerLimitWeight            
    ]          
  ]
end

to-report applyMutation [gene]
  ifelse random-float 1 <= mutationRate [ report mutatedValue gene ][ report gene ]
end

to-report mutatedValue [gene]
  ;; mutation happens
  ifelse random-float 1 <= probabilityUniformMutation 
    [ ;; the new value comes from a uniform distribution
      ifelse lowerLimitWeight < 0 [set gene (random-float (2 * upperLimitWeight)) + lowerLimitWeight]
      [set gene (random-float upperLimitWeight)]
    ]
    [ ;; the new value comes from a normal distribution
      set gene gene + random-normal 0 mutationStd
    ]
  if gene > upperLimitWeight [set gene upperLimitWeight] ;; if we go outside the bounds
  if gene < lowerLimitWeight [set gene lowerLimitWeight]  ;; if we go outside the bounds
  
  report gene
end

to-report neuralNetwork [vectorV vectorW listOfInputs]
  let vectorH []
  foreach vectorV ;; for each input
  [
    let hValue []
    let subVector ?
    set hValue sum (map * subVector listOfInputs) ;;linear combination
    set hValue 1 / (1 + e ^ (- hValue)) ;; sigmoid
    set vectorH lput hValue vectorH
  ]
  let yValue sum (map * vectorW vectorH) ;;linear combination
  set yValue 1 / (1 + e ^ (- yValue)) ;; sigmoid
  report yValue
end

to printToFile
  if file-exists? outputFile [ file-close file-delete outputFile ]  
  file-open outputFile
  foreach globalListNames [ file-write ? ] file-print ""  ;; print global variables names
  foreach globalList [ file-write ? ] file-write (timer / 3600) file-print ""  ;; print their value  
  foreach outputListNames [ file-write ? ] file-print ""  ;; print interesting variables names
  foreach outputList ;; for each recorded generation
  [
    let mySublist ?
    foreach mySublist [ file-write formatListToPrint ? ] file-print ""
  ]
  file-close
  
  let outputFile2 (word "networks_" seed ".txt")
  if file-exists? outputFile2 [ file-close file-delete outputFile2 ]  
  file-open outputFile2
  ifelse geneticImplementation = "neuralNetwork" 
  [
    file-write "poidsVOffer" file-write "poidsWOffer" file-write "poidsVRequest" file-write "poidsWRequest" file-write "timesResp" file-write "timesProp" file-print "" 
    ask turtles [file-write formatListToPrint poidsVOffer file-write formatListToPrint poidsWOffer file-write formatListToPrint poidsVRequest file-write formatListToPrint poidsWRequest file-write timesResp file-write timesProp file-print ""]
  ]
  [
    file-write "p11" file-write "p22" file-write "p12" file-write "p21" file-write "q11" file-write "q22" file-write "q12" file-write "q21" file-write "timesResp" file-write "timesProp" file-print "" 
    ask turtles [file-write p11 file-write p22 file-write p12 file-write p21 file-write q11 file-write q22 file-write q12 file-write q21 file-write timesResp file-write timesProp file-print "" ]
      
  ]
  file-close  
  
  ;let outputFile3 (word "productionsPaired_" seed ".txt")
  ;if file-exists? outputFile3 [ file-close file-delete outputFile3 ]  
  ;file-open outputFile3
  ;file-write "productionsPaired" file-print "" 

  ;foreach productionsPaired [ file-write formatListToPrint ? file-print "" ] 


  file-close  
end

to-report formatListToPrint [myList]
  
  let myWord (word "" myList)
  
  while [position " " myWord != false] 
  [
    let myPosition position " " myWord
    set myWord replace-item myPosition myWord ","  
  ]
  while [position "[" myWord != false] 
  [
    let myPosition position "[" myWord
    set myWord replace-item myPosition myWord "{"  
  ]  
  while [position "]" myWord != false] 
  [
    let myPosition position "]" myWord
    set myWord replace-item myPosition myWord "}"  
  ]  
  report myWord  
end
@#$#@#$#@
GRAPHICS-WINDOW
218
11
463
230
16
16
5.7
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
112
29
186
63
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
39
29
103
63
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
197
474
309
519
population size
count turtles
17
1
11

BUTTON
72
68
154
101
go one
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
346
418
429
463
generation
ticks
17
1
11

MONITOR
343
474
410
519
Time (s)
timer
17
1
11

MONITOR
199
415
332
460
time in generation
timeElapsed
17
1
11

INPUTBOX
19
556
180
616
beta
1
1
0
Number

INPUTBOX
24
617
185
677
tau
0.01
1
0
Number

INPUTBOX
24
306
185
366
updatePlotsEvery
1
1
0
Number

MONITOR
1473
460
1690
505
mean payoff of Weak vs strong
mean payoffResponder21
17
1
11

PLOT
437
259
775
474
Offers accepted by Weak vs Weak
Offer
Number
0.0
1.0
0.0
500.0
false
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram payoffResponder11"

PLOT
1147
731
1459
935
Mean payoff responder strong vs weak
Generations
Payoff
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if length payoffResponder12 > 0 [plotxy ticks mean payoffResponder12]"

INPUTBOX
26
368
187
428
mutationRate
0.0033
1
0
Number

INPUTBOX
22
170
183
230
numberOfGenerations
22000
1
0
Number

INPUTBOX
19
491
180
551
lifespan
500
1
0
Number

PLOT
796
260
1137
476
Offers rejected by Weak vs Weak
NIL
NIL
0.0
1.0
0.0
4000.0
true
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram rejectedOffers11"

CHOOSER
228
737
366
782
recombination
recombination
"Yes" "No"
0

INPUTBOX
24
104
185
164
populationSize
300
1
0
Number

INPUTBOX
209
272
370
332
pStart
0
1
0
Number

INPUTBOX
207
341
368
401
qStart
0
1
0
Number

INPUTBOX
22
682
183
742
numberOfHiddenNeurons
4
1
0
Number

CHOOSER
230
599
420
644
geneticImplementation
geneticImplementation
"neuralNetwork" "pq" "gamma"
2

INPUTBOX
19
745
180
805
strength1
0
1
0
Number

INPUTBOX
20
807
181
867
strength2
1
1
0
Number

INPUTBOX
18
431
179
491
mutationStd
0.3
1
0
Number

INPUTBOX
22
229
183
289
probabilityUniformMutation
0
1
0
Number

PLOT
1147
490
1460
720
Mean payoff responder Weak vs Strong
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if length payoffResponder21 > 0 [plotxy ticks mean payoffResponder21]"

PLOT
438
10
777
249
Offers accepted by Strong vs Strong
NIL
NIL
0.0
1.0
0.0
500.0
false
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram payoffResponder22"

PLOT
795
13
1133
251
Offers rejected by Strong vs Strong
NIL
NIL
0.0
1.0
0.0
4000.0
true
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram rejectedOffers22"

MONITOR
1468
20
1677
65
Mean payoff Strong vs Strong
mean payoffResponder22
17
1
11

PLOT
438
483
776
711
Offers accepted by Weak vs Strong
NIL
NIL
0.0
1.0
0.0
500.0
false
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram payoffResponder21"

PLOT
796
486
1133
714
Offers rejected by Weak vs Strong
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram rejectedOffers21"

PLOT
442
727
777
933
Offers accepted by Strong vs Weak
NIL
NIL
0.0
1.0
0.0
500.0
false
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram payoffResponder12"

PLOT
799
733
1130
936
Offers rejected by Strong vs Weak
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"set-plot-pen-interval 0.01" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram rejectedOffers12"

PLOT
1145
16
1461
249
mean payoff responder Strong vs Strong
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if length payoffResponder22 > 0 [plotxy ticks mean payoffResponder22]"

PLOT
1146
260
1462
481
mean payoff responder Weak vs Weak
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if length payoffResponder11 > 0 [plotxy ticks mean payoffResponder11]"

MONITOR
1468
239
1658
284
mean payoff weak vs weak
mean payoffResponder11
17
1
11

MONITOR
1465
766
1682
811
mean payoff of strong vs weak
mean payoffResponder12
17
1
11

INPUTBOX
19
878
180
938
probaWeak
0.1
1
0
Number

CHOOSER
220
668
358
713
continuum
continuum
"No" "Yes"
1

@#$#@#$#@
## WHAT IS IT?

Netlogo simulation for the evolution of fairness by partner choice, with asymmetries of strength between partners.

Model explained and results published in "Evolution of equal division among unequal partners", Evolution, 2014. http://stephanedebove.net/evolution-of-equal-division-among-unequal-partners/

If more details are needed on how this model works, please contact the first author of the publication (find contact on personal website at http://stephanedebove.net ).

Code released under Gnu GPL license

@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
