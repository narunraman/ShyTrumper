extensions[nw]

breed [voters voter]

turtles-own [ x y shyness dist_voter_candidate demographics vote_pref times_asked ]

globals [candidates voter_colors poll_vcount poll_cycle_count neighborhood incumbent election]

to setup
  clear-all
  set-default-shape turtles "person"

  ifelse neighborhood_type = "Von Neumann"
    [ set neighborhood von-neumann-offsets neighborhood_radius false ]
    [ set neighborhood moore-offsets neighborhood_radius false ]

  ask patches [
    sprout-voters 1 [
      set shyness random-float 1
    ]
  ]

  ; generate 2-d list of candidate. Each candidate has an x position, y position, and a color
  set candidates []
  set candidates init-list-count candidates number_of_candidates true 3
  ask voters [
    set demographics []
    ifelse neighborhood_type = "Von Neumann" [
      set demographics init-list-count (demographics) (2 * neighborhood_radius * (neighborhood_radius + 1)) false 0
    ]  [
      set demographics init-list-count (demographics) (((2 * neighborhood_radius + 1) ^ 2) - 1) false 0
    ]
  ]
  orient-candidates-voters [ -1 ]
  set election []
  set poll_vcount []
 reset-ticks
end

to orient-candidates-voters [ incumbent? ]
  ; generate 2-d list of candidate. Each candidate has an x position, y position, and a color
  let i 0
  ; the demographics for each voter is a list of tuples
  repeat number_of_candidates [
    ifelse use-normal-distribution
    [ let candidate []
      if incumbent? != i [
        set candidate (list (random-normal 0 standard-dev) (random-normal 0 standard-dev) (5 + 20 * i))
        if item 0 candidate < -1 [set candidate replace-item 0 candidate -1]
        if item 0 candidate >  1 [set candidate replace-item 0 candidate  1]
        if item 1 candidate < -1 [set candidate replace-item 1 candidate -1]
        if item 1 candidate >  1 [set candidate replace-item 1 candidate  1]
        set candidates replace-subitem i 0 candidates (item 0 candidate)
        set candidates replace-subitem i 1 candidates (item 1 candidate)
        set candidates replace-subitem i 2 candidates (item 2 candidate)
;        set candidates replace-item i candidate candidates
      ]
     ]
    [ if incumbent? != i [
      set candidates replace-subitem i 0 candidates (random-float 2 - 1)
      set candidates replace-subitem i 1 candidates (random-float 2 - 1)
      set candidates replace-subitem i 2 candidates (5 + 20 * i)
;        set candidates replace-item i (list (random-float 2 - 1) (random-float 2 - 1) (5 + 20 * i)) candidates
      ]
    ]
    set i i + 1
  ]

  ; set up a list of distances to all candidates for each voter
  ask voters [
    set shape "person"
    let neighborhoodDemo demographics
    let n 0
    ask patches at-points neighborhood [
      ask turtles-on self [
        set neighborhoodDemo replace-item n neighborhoodDemo (list who 0)
      ]
      set n n + 1
    ]
    set demographics neighborhoodDemo
    set dist_voter_candidate []
    set i 0
    repeat number_of_candidates [
      ifelse use-normal-distribution
        [ set x random-normal 0 standard-dev
          if x < -1 [set x -1]
          if x > 1 [set x 1]
          set y random-normal 0 standard-dev
          if y < -1 [set y -1]
          if y > 1 [set y 1]
        ]
        [set x random-float 2 - 1
          set y random-float 2 - 1]
        set dist_voter_candidate lput (1 - sqrt(((item 0 item i candidates - x) ^ 2) + (item 1 item i candidates - y) ^ 2)) dist_voter_candidate

       set i i + 1
    ]
    ; set all voters to be the color of the candidate closest to them
    let max_score -10
    let color_num 0
    set i 0
    repeat number_of_candidates [
      if  item i dist_voter_candidate > max_score [
          set max_score item i dist_voter_candidate
          set color item 2 item i candidates
        ]
        set i i + 1
    ]
  ]
end

to step
  go
end

to go
;
  if ticks mod poll_cycle = 0[
    release-poll
    set poll_cycle_count poll_cycle_count + 1
  ]
  if ticks mod election_cycle = election_cycle - 1 [
    vote
    tick
  ]
  if ticks mod election_cycle != election_cycle - 1 [
    update-voters
    poll-neighbor
    tick
  ]
end

to vote
  let election_result []
  set election_result (init-list-count election_result number_of_candidates false 0)
  ask voters [
    let vote_thresh random-float 1
    ifelse vote_pref > vote_thresh [
      set election_result replace-item ((color - 5) / 20) election_result (item ((color - 5) / 20) election_result + 1)
    ] [
      set shape "x"
      set color white
    ]
  ]
  set election election_result
  set incumbent num-to-color (5 + (max-index election_result) * 20)
  orient-candidates-voters max-index election_result
end

to-report num-to-color [ n ]
  if n = blue [report "blue"]
  if n = grey [report "grey"]
  if n = orange [report "orange"]
  if n = cyan [report "cyan"]
  if n = lime [report "lime"]
  if n = yellow [report "yellow"]
  if n = magenta [report "magenta"]
end

to-report max-index [ l ]
  let m max l
  let i 0
  foreach l [
    z -> ifelse z = m[ report i ] [ set i i + 1 ]
  ]
end

to-report new-incumbent
  if incumbent != 0 [
    report (word "The " incumbent " candidate wins!")
  ]
end

; Initializes a list with a set number of values
; If a demographic list then instantiate with a list of tuples
to-report init-list-count [ l n list_of_lists num_in_list]
  repeat n [
    ifelse list_of_lists [
      let list_in_list []
      repeat num_in_list [ set list_in_list lput (0) list_in_list]
      set l lput(list_in_list) l
    ][
      set l lput(0) l
    ]
  ]
  report l
end

; Polls a percent of voters, with probability of voters responding being their shyness
to release-poll
  let respond? random-float 1
  let candidate_vcount []
  set candidate_vcount (init-list-count candidate_vcount number_of_candidates false 0)
  ask n-of ((count voters) / percent_polled) voters [
    if respond? < shyness [
      set candidate_vcount replace-item ((color - 5) / 20) candidate_vcount (item ((color - 5) / 20) candidate_vcount + 1)
    ]
  ]

  set poll_vcount candidate_vcount
end

to-report any?-zero [ l ]
  foreach l [
    z -> if z = 0 [report true]
  ]
  report false
end

; updates the voters with the results of the poll and set the new relation of candidates
; for a calculation for vote preference
to update-voters
  let candidate_relation []
  set candidate_relation (init-list-count candidate_relation number_of_candidates false 0)
  let max_candidate max poll_vcount
  if poll_cycle_count > 1 [
    ask voters [
      let i 0
      repeat number_of_candidates [
        set candidate_relation replace-item i candidate_relation (item i poll_vcount - max_candidate)
        set i i + 1
      ]
    ]
  ]
  ask voters [
    while [any?-zero (poll_vcount)] [
      release-poll
    ]
    set vote_pref update-vote-pref candidate_relation poll_vcount shyness
  ]
end

to-report update-vote-pref [ cand_rel poll_result shy_val ]
  report 1 - (abs(item ((color - 5) / 20) cand_rel) / (item ((color - 5) / 20) poll_result + max(poll_result)) + shy_val / 2)
end

; asks neighbor about ideology and respond back depending upon characteristics
to poll-neighbor
  ask voters[
    let ideo color
    let currDemo demographics
    ask turtles-on one-of patches at-points neighborhood[

      ; Lie if majority ideology is not same as self
      ; otherwise tell the truth
      ifelse maj-demo-freq demographics (maj-demo demographics) > check-similar-ideo demographics color [
        ifelse random-float 1 > shyness [
          foreach [demographics] of myself [
            z -> if item 0 z = who [ set z replace-item 1 z maj-demo demographics ]
          ]
          ask patch-here [
            set pcolor maj-demo ([demographics] of myself)
          ]
        ] [
          foreach [demographics] of myself [
            z -> if item 0 z = who [ set z replace-item 1 z color ]
          ]
        ]
      ] [
          foreach [demographics] of myself [
            z -> if item 0 z = who [ set z replace-item 1 z color ]
          ]
      ]
      ifelse color != ideo and shyness < (check-similar-ideo currDemo ideo) [
        ; IF NOT SAME IDEOLOGY AND LARGE ENOUGH FRACTION OF PEOPLE SAME IDEO TO SURPASS SHYNESS
        ifelse check-similar-ideo currDemo color < maj-demo-freq (currDemo) (maj-demo currDemo)[
          ; IF THE MAJORITY IDEOLOGY IS NOT NEIGHBOR's, LIE WITH MAJORITY IDEOLOGY
          exchange-ideo myself self 0
        ]

        ; IF THE MAJORTIY IDEOLOGY IS NEIGHBORS's, SO LIE WITH NEIGHBORS IDEOLOGY
        [
          exchange-ideo myself self 1
        ]
      ]

      ; IF SAME IDEOLOGY OR SHYNESS IS GREATER THAN FRACTION OF PEOPLE WITH SAME IDEOLOGY
      [
        exchange-ideo myself self 2
      ]
    ]
  ]
end

to-report maj-demo-freq [neighborsDemo mode]
  let freq 0
  foreach neighborsDemo [
    z -> if(item 1 z = mode) [
      set freq freq + 1
    ]
  ]
  report freq
end

; checks list of tuples for similar ideologies and returns percent of neighborhood
to-report check-similar-ideo [ neighborsDemo iD ]
  if num-asked-neighbors neighborsDemo = 0 [
    report 0
  ]
  let ideo_similar 0
  foreach neighborsDemo [
    z -> if item 1 z = iD [
      set ideo_similar ideo_similar + 1
    ]
  ]
  report ideo_similar / num-asked-neighbors neighborsDemo
end

to-report num-asked-neighbors [ neighborDemo ]
  let num 0
  foreach neighborDemo [
    z -> if item 1 z != 0 [
      set num num + 1
    ]
  ]
  report num
end

; normalize the variance of the ideologies and returns as a disparity index
to-report disparity-of-demographics [ neighborDemo ]
  if num-asked-neighbors neighborDemo < 2 [
    report 0
  ]
  let colors []
  foreach neighborDemo [
    z -> if item 1 z != 0 [
      set colors lput(item 1 z) colors
    ]
  ]
  report (variance colors / get-mean-demo neighborDemo)
end

to-report get-mean-demo [ neighborDemo ]
  let m []
  foreach neighborDemo [
    z -> if item 1 z != 0 [
      set m lput(item 1 z) m
    ]
  ]
  report mean m
end

to-report maj-demo [ neighborDemo ]
  let demo []
  foreach neighborDemo [
    z -> if item 1 z != 0 [
      set demo lput item 1 z demo
    ]
  ]
  report modes demo
end

to-report new-shyness [ percent_diff demo_variance prev_shyness]
  let new_shyness ((prev_shyness + percent_diff / 10 )) ;+ demo_variance))
  if new_shyness < 0 or new_shyness > 1[
    report random-float 1
  ]
  report new_shyness
end

; This function sets the values of the ideological demographics for each voter and neighbor they talk to
; Includes the rules of when might the voter lie back to the neighbor as denoted by integer lie?
to exchange-ideo [ person neighbor lie? ]
  let neighborsDemo [demographics] of neighbor
  let percent_similar_old 0
  let disparity 0
  if num-asked-neighbors neighborsDemo > 0[
    set percent_similar_old check-similar-ideo neighborsDemo [color] of neighbor
  ]
  if num-asked-neighbors neighborsDemo > 1[
    set disparity disparity-of-demographics neighborsDemo
  ]
  let i 0
  foreach [demographics] of neighbor [
    z -> if item 0 z != [who] of person [
      set i i + 1
    ]
  ]
  ; Depending on value of lie? then exchange value accordingly
  ; Update color of patch if lie
  if lie? = 0 [
    ask neighbor [
      set demographics replace-subitem i 1 demographics (maj-demo neighborsDemo)
        set shyness new-shyness (percent_similar_old - check-similar-ideo demographics (maj-demo neighborsDemo)) disparity shyness
      ask patch-here [
        set pcolor (maj-demo neighborsDemo)
      ]
    ]
  ]
  if lie? = 1 [
    ask neighbor [
      set demographics replace-subitem i 1 demographics color
      set shyness new-shyness (percent_similar_old - check-similar-ideo demographics color) disparity shyness
      ask patch-here [
        set pcolor [color] of person
      ]
    ]
  ]
  if lie? = 2 [
    ask neighbor [
      set demographics replace-subitem i 1 demographics [color] of person
        set shyness new-shyness (percent_similar_old - check-similar-ideo demographics [color] of person) disparity shyness
      ask patch-here [
        set pcolor black
      ]
    ]
  ]
end

to-report replace-subitem [index1 index2 lists value]
  let old-sublist item index1 lists
  report replace-item index1 lists (replace-item index2 old-sublist value)
end

to-report moore-offsets [n include-center?]
  let result [list pxcor pycor] of patches with [abs pxcor <= n and abs pycor <= n]
  ifelse include-center?
    [ report result ]
    [ report remove [0 0] result ]
end

to-report von-neumann-offsets [n include-center?]
  let result [list pxcor pycor] of patches with [abs pxcor + abs pycor <= n]
  ifelse include-center?
    [ report result ]
    [ report remove [0 0] result ]
end

to-report num-in-list [ l ]
  let s 0
  foreach l [
    z -> set s s + z
  ]
  report s
end

to change_voter

end

to plot-vote-preference
  clear-plot
  set-plot-x-range 0 (count voters)
  set-plot-y-range 0 1
  set-current-plot-pen "pen-0"
  ;let avg-pr-vote []
  let m 0
  repeat number_of_candidates [
    let vote_pref_color []
    ask voters with [color = 5 + m * 20] [
      set-plot-pen-color 5 + m * 20
      plot vote_pref
      if m = number_of_candidates - 1[
;        show vote_pref
      ]
;      set vote_pref_color lput (vote_pref) vote_pref_color
    ]
;    show vote_pref_color
;    repeat 100 [
;      set-plot-pen-color 5 + m * 14
;      plot median vote_pref_color
;    ]
    ;plot item m avg-pr-vote
    set m m + 1
  ]
end

to plot-total-voter-turnout
  let curr_election []
  if ticks mod election_cycle = 0[
    set curr_election election
  ]
  if length curr_election > 0 [
    repeat 100 [
      set-plot-pen-color black
      plot (num-in-list curr_election / count voters) * 100
    ]
  ]
  set curr_election []
end

to plot-voter-turnout
  let curr_election []
  if ticks mod election_cycle = 0 [
    set curr_election election
    clear-plot
  ]
  if length curr_election > 0[
    set-plot-x-range 0 (number_of_candidates) ;+ 1
    set-plot-y-range 0 100
    set-current-plot-pen "pen-0"
    let m 0
    foreach curr_election [
      z ->
      repeat 100 [
        set-plot-pen-color 5 + m * 20
        plot (z / num-in-list curr_election) * 100
      ]
      set m m + 1
    ]
;    repeat 100 [
;      set-plot-pen-color black
;      plot (num-in-list election / count voters) * 100
;    ]
  ]
  set curr_election []
end
@#$#@#$#@
GRAPHICS-WINDOW
253
10
690
448
-1
-1
13.0
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
0
0
1
ticks
30.0

BUTTON
14
76
80
109
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
91
76
154
109
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
1

SLIDER
20
130
218
163
Number_of_Candidates
Number_of_Candidates
2
7
4.0
1
1
NIL
HORIZONTAL

SWITCH
19
180
228
213
use-normal-distribution
use-normal-distribution
1
1
-1000

SLIDER
35
227
207
260
standard-dev
standard-dev
.1
1
0.3
.1
1
NIL
HORIZONTAL

SLIDER
33
330
207
363
Neighborhood_Radius
Neighborhood_Radius
1
5
5.0
1
1
NIL
HORIZONTAL

SLIDER
35
376
207
409
Poll_Cycle
Poll_Cycle
5
20
5.0
1
1
NIL
HORIZONTAL

CHOOSER
45
273
196
318
Neighborhood_Type
Neighborhood_Type
"Von Neumann" "Moore"
1

PLOT
710
12
870
132
Voter Preferences
NIL
NIL
-1.0
1.0
-1.0
1.0
false
false
"let xs [x] of voters\nlet ys [y] of voters\nlet voter-color [color] of voters\nlet m 0\nset-current-plot-pen \"pen-0\"\nwhile [m < length xs]\n  [set-plot-pen-color item m voter-color\n   plotxy item m xs item m ys\n   set m m + 1]" ""
PENS
"pen-0" 1.0 2 -16777216 true "" ""

PLOT
709
142
948
325
Poll Statistics
Candidates
Percentage of Vote
0.0
5.0
0.0
100.0
false
false
"" "clear-plot\nlet m 0\nset-plot-x-range 0 number_of_candidates\nset-current-plot-pen \"pen-0\"\nlet total_polled 0\nif length poll_vcount > 0 [\n foreach poll_vcount [\n   z -> set total_polled total_polled + z  \n ]\n while [m < length poll_vcount]\n   [set-plot-pen-color 5 + m * 20\n    repeat 100 [\n      plot (item m poll_vcount / total_polled) * 100\n    ]\n    set m m + 1]\n]\n"
PENS
"pen-0" 0.01 1 -7500403 true "" ""

PLOT
881
13
1041
133
Candidate Position
NIL
NIL
-1.0
1.0
-1.0
1.0
false
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" "clear-plot\nlet n 0\nset-current-plot-pen \"pen-0\"\nwhile [ n < number_of_candidates]\n  [plot-pen-down \n   plotxy 0 0\n   set-plot-pen-color 5 + 20 * n\n   plotxy (item 0 item n candidates) (item 1 item n candidates)\n   set n n + 1]"

SLIDER
35
453
207
486
Election_Cycle
Election_Cycle
30
200
75.0
1
1
NIL
HORIZONTAL

PLOT
959
142
1210
325
Actual Statistics
Candidates
Percentage of Vote
0.0
5.0
0.0
100.0
false
false
"" ""
PENS
"pen-0" 0.01 1 -16777216 true "clear-plot\nlet m 0\nset-plot-x-range 0 number_of_candidates\nset-current-plot-pen \"pen-0\"\nwhile [m < number_of_candidates]\n  [set-plot-pen-color 5 + m * 20\n   repeat 100 [\n     plot (count voters with [color = 5 + m * 20] / (count voters)) * 100\n   ]\n   set m m + 1]\n   \n   " ""

PLOT
709
334
948
517
Vote Preference by Candidate
Voters
Probability to Vote
0.0
3.0
0.0
1.0
false
false
"" ""
PENS
"pen-0" 1.0 1 -16777216 true "" "plot-vote-preference"

BUTTON
170
75
233
108
NIL
step\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
253
462
455
511
Incumbent
new-incumbent
17
1
12

PLOT
960
334
1209
516
Turnout
Candidates
Percentage of Vote
0.0
5.0
0.0
100.0
true
false
"" ""
PENS
"pen-0" 0.01 1 -16777216 true "" "plot-voter-turnout"

PLOT
1049
13
1209
133
Total Turnout
Election Year
% Voting
0.0
1.0
0.0
100.0
true
false
"" ""
PENS
"pen-0" 0.01 1 -16777216 true "" "plot-total-voter-turnout"

SLIDER
36
414
208
447
Percent_Polled
Percent_Polled
0
100
10.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model intends to reveal the impact of certain opaque variables surrounding voter turnout. In this instance we turn to the election of 2016 and the idea of a "shy-Trumper". This idea was based on the fact that Trump was seen as a morally corrupt individual and therefore when polled by a live interviewer Trump supporters were less likely to respond truthfully, or respond at all. This may have incurred a non-response bias within our polling system and led to the great disparity between what was projected and what actually occured. So this model serves to shed some light on that idea and hopefully see what some of the biases that may have played a part in the election.

## HOW IT WORKS

Voters and Candidates are assigned a random point in a 2 dimensional unit plot. Voters are assigned a candidate they most closely align with, according to Euclidean distance. 
At each tick a voter, one of the turtles in the visualization space, asks one of their neighbors regarding their ideology. The neighbor can respond in by either lying if his neighborhood's majority ideological demographic is different than his, or telling the truth. With this information about his neighbor (whether accurate or not) the voter decides how he would like to share his own ideology back. If his ideology is a minority in his own neighborhood then the voter might lie. There are two options for this, if the received ideology is the voter's neighborhood's majority ideological demographic then lie with same ideology as received ideology, otherwise lie with majority ideology. If he is not a minority in his own neighborhood and his shyness is high enough then he will respond with the truth. 
Additionally, after an exogenously determined number of ticks, a poll will be released (a random percentage of the population will be polled) detailing gathered data about candidate's perceived support level. Voters get polled at this stage, and if their shyness is low enough then they may not respond, skewing the poll even further.
Lastly, after an exogenously determined amount of ticks, there is a general election and voters will vote depending on their vote preference. This is determined by their shyness and how well their candidate is doing in relation to the others (seen by the released poll). The greater the distance a candidate is to the best performing candidate the less likely to vote, the closer their candidate is the more likely to vote.

## HOW TO USE IT

To setup the model, adjust the sliders to any values you like and then hit setup and then go. To see one step at a time, hit step. 

## THINGS TO NOTICE

At each election cycle there will be an election, and in the graphs on the right-most side, the results are shown. In the top right-hand-side is the total turnout over multiple elections. Often this mirrors the US elections' turnouts. On the bottom right-hand-side is the turnout for each candidate in the current election.

## THINGS TO TRY

Try to play around with combinations of poll_cycle and election_cycle to see if you notice any differences. There might be different results if there is a lot of polls released before any election vs. one or two. Also try to see if different neighborhood sizes and shapes affect the outcomes.

## EXTENDING THE MODEL

Adding further genetic algorithms for the voters would be interesting, for example, if your candidate was projected to do win by a lot but the candidate did not win (i.e. low turnout for that candidate as people found less marginal utility in voting) they might change their strategy in the next election to remedy that. 

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.0.4
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
