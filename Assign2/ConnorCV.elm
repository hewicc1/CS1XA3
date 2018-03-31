
import Html exposing (..)
import Html.Attributes exposing (..)

divStyle = style [("background-image", "url(b.jpg)")
                  ,("border-style","solid")
                  ,("font-family","Times New Roman")
                  ,("padding","0 20% 1em 20%")]

headerStyle = style [("text-align","center")
                    ,("background-color","orange")
                    ,("color","white")
                    ,("border-style","dotted")]
                   
mainStyle = style [("color","white")
                  ,("background-color","rgba(0, 0, 0, 0)")]
                    
footerStyle = style [("text-align","center")
                    ,("color","white")]
              
skillsStyle = style [("float","right")
                    ,("color","white")
                    ,("padding-left","75%")]               

main = div [divStyle] [
              
              header [headerStyle] [
                                    h1 [] [text "Connor Hewick"]
                                    ,text "hewicc1@mcmaster.ca"]

              ,section [mainStyle] [
                                    h3 [] [text "Objective:"]
                                    , p [] [text "I am a university student who has passion to learn new skills, techniques and how things work within the professional world. I am able to work and collaborate within a team environment, as well as work individually while striving to be time efficient and maintain work quality. I am most comformable with programming with Python and Swift."]
                                    ]

              ,img [src "/connor123.jpg",style [("width","100%")]] []

              ,aside [skillsStyle] [
                          text "Skills:"
                          ,ul [] [
                                li [] [text "Using Python"]
                                ,img [src "4_stars.png",style [("width","100%")]] []
                                ,li [] [text "Using Swift"]
                                ,img [src "3.5_stars.png",style [("width","100%")]] []
                                ,li [] [text "Collaboration"]
                                ,img [src "4_stars.png", style [("width","100%")]] []
                                ,li [] [text "Organization"]
                                ,img [src "4_stars.png", style [("width","100%")]] []
                                ,li [] [text "Responibility"]
                                ,img [src "4_stars.png", style [("width","100%")]] []
                                ]
                          ]

              ,section [mainStyle] [
                                       h3 [] [text "Education:"]
                                       , ul [] [
                                               li [] [text "McMaster University, Hamilton, Computer Science (2017-present)"]
                                               ,li [] [text "Grades 9-12 Sherwood Secondary School, Hamilton, Ontario"]
                                               ]  
                                       ]

              ,section [mainStyle] [
                                    h3 [] [text "Awards/Achievments:"]
                                    ,ul [] [
                                            li [] [text "C.T Lowe Award for School Involvement, 2017"]
                                            ,li [] [text "Academic Honours, 2012-17"]
                                            ] 
                                    ]

              ,section [mainStyle] [
                                        h3 [] [text "Work Expirence:"]
                                       , ul [] [
                                               li [] [text "Fortinos (September 2015 - present 2018)"
                                                      ,ul [] [
                                                              li [] [text "dairy clerk"]
                                                              ,li [] [text "stocked shelves of milk, eggs, yogurt and juice"]
                                                              ,li [] [text "maintenance duties including organizing skids in back room"]
                                                              ]
                                                        ]
                                               ,li [] [text "McDonalds (June 2015 - September 2015)"
                                                        ,ul [] [
                                                                li [] [text "kitchen crew member"]
                                                                ,li [] [text "prepared burgers, wraps and fries"]
                                                                ,li [] [text "maintenance duties including taking out garbage and cleaning"]
                                                                ]
                                                        ]
                                               ]  
                                        ]

              ,section [mainStyle] [
                                    h3 [] [text "Computer Skills:"]
                                    , ul [] [
                                            li [] [text "Familiar with Python, Swift, Visual Basic, Haskell, Bash Scripting, IOS, Windows"]
                                            ,li [] [text "Certificate of Completion for IT Essentials (Cisco Networking Academy)"]
                                            ,li [] [text "Successfully completed 25 problems on https://projecteuler.net"]
                                            ,li [] [text "Used the Elm architecture to create ",a [href "Scared_Elephant.html"] [text "The Scared Elephant"]]
                                            ,li [] [text "Link to alternate CV page created with Elm" ,a [href "index2.html"] [text "Here"]]
                                            ,li [] [text "Link to personal GitHub ",a [href "https://github.com/connorhewick"] [text "here"]]
                                            ]
                                    ]
              
              ,footer [footerStyle] [
                                    text "References are available upon request."
                                    ]
              ]
              
        