module WebApp exposing (..)

import Html as Html
import Html.Attributes as Att
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseMove)
import Keyboard as Key
import Mouse as Mouse


{- MODEL: tracks x & y of mouse position and x & y of keyboard position -}
type alias Model = { mousePosition : { x : Int, y : Int} , keyPosition : { x : Int, y : Int }, score : Int}

{- INITIAL position is both svg's centered with mouse svg above keyboard svg -}
init : ( Model, Cmd Msg )
init = ( { mousePosition = { x = 650, y = 150} , keyPosition = { x = 650, y = 300 }, score = 0}, Cmd.none )

{- MESGGAGES: sends either the mouse position or the keyboard keycode -}
type Msg = MouseMsg Mouse.Position 
            | KeyMsg Key.KeyCode
            | CollisionMsg

{- VIEW: using html displays intro text and both svg shapes -}
view : Model -> Html.Html Msg
view model = let
            -- correct the offset on the mouse svg cause from the intro text
            mousePositionX = toString (model.mousePosition.x-33)
            mousePositionY = toString (model.mousePosition.y-120)
            keyPositionX = toString model.keyPosition.x
            keyPositionY = toString model.keyPosition.y
    in Html.div [Att.style [("width","100%"), ("height","700px"),("text-align","center"),("background-color","yellow")]] [
                Html.header [] [
                Html.pre [] [Html.text "Welcome to The Scared Elephant: The Two Player Game \n Can the 'elephant'(grey ellipse) avoid the 'mouse'(computer mouse)? \n Player one controls the mouse using the mouse and player two controls the elephant using 'w,a,s,d' or the arrow keys. 'r' can be used to reset the score. \n Points are awarded if the mouse can run through the elephants legs (i.e. move the mouse through the ellipse). \n Created by Connor Hewick, 2018."]
                ,Html.text "Score: ",Html.text ((toString model.score))
                                ]
                ,svg  [width "95%", height "80%"] [
                                                    ellipse [cx keyPositionX, cy keyPositionY, rx  "200" ,ry "50",fill "gray", onMouseMove CollisionMsg] []
                                                    ,circle [cx mousePositionX, cy mousePositionY, r "3",fill "brown"] []
                                                    ]
                                                                                                                            ]

{- UPDATE: moves either mouse svg, keyboard svg, detects a collision or resets score based on the message recieved -}
update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model = case msg of
                MouseMsg position ->
                            ({model | mousePosition = { x = position.x, y = position.y}, keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y}, score = model.score}, Cmd.none)
                KeyMsg keycode -> case keycode of
                                68 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x + 75 , y = model.keyPosition.y}, score = model.score}, Cmd.none )
                                39 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x + 75 , y = model.keyPosition.y}, score = model.score}, Cmd.none )
                                65 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x - 75 , y = model.keyPosition.y}, score = model.score}, Cmd.none )
                                37 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x - 75 , y = model.keyPosition.y}, score = model.score}, Cmd.none )
                                83 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y + 75}, score = model.score}, Cmd.none )
                                40 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y + 75}, score = model.score}, Cmd.none )
                                87 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y - 75}, score = model.score}, Cmd.none )
                                38 -> ( {model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y}, keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y - 75}, score = model.score}, Cmd.none )
                                82 -> ({ model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y} , keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y }, score = 0}, Cmd.none)
                                _ -> (model, Cmd.none)
                CollisionMsg -> ({ model | mousePosition = { x = model.mousePosition.x, y = model.mousePosition.y} , keyPosition = { x = model.keyPosition.x, y = model.keyPosition.y }, score = model.score+1}, Cmd.none)
                            
{- SUBSCRIPTIONS: subscribed to follow mouse moves and keyboard downs -}
subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ Mouse.moves MouseMsg
                                , Key.downs KeyMsg]

{- MAIN: program structure -}
main : Program Never Model Msg
main = Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }