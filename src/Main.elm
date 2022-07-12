module Main                              exposing (main)

import Html                              exposing (..)
import Html.Events                       exposing (..)
import Parser as P                       exposing (..)
import Url                               exposing (Url)
import Html.Attributes as Attr           exposing (..)
import Url.Parser as UrlParser           exposing ((</>), Parser, s, top)
import Browser                           exposing (UrlRequest)
import Http
import Json.Encode as E
import Json.Decode as D
import Browser.Navigation as Navigation
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col





main = 
   Browser.application
      { init   = init
      , update = update
      , subscriptions = subscriptions
      , view = view
      , onUrlRequest = ClickedLink
      , onUrlChange = UrlChange
      }

type alias Flags = 
   {}

subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.none




-- DATATYPE DECLARATIONS




type Request = None
             | Failure
             | Success String

type Page = LandingPage  -- the website's landing page
          | Home         -- the homepage delivering the main search functionality
          | Login        -- user must log in before they can access their notes
          | Stack        -- page containing the list of notes that the user has saved
          | NotFound     -- if the route is wrong this page will be displayed 

type alias Credentials = 
   { email    : String
   , password : String                      
   }

type Source = Cheatsh   -- the source is https://cheat.sh
            | ManPages  -- the source is the official linux man pages
            | TLDRPages -- the source is the tldr pages	




-- MODEL




type alias Model = 
   { contentRequest  : Request        -- the result of the post request to the API
   , navKey          : Navigation.Key -- key used for navigating between pages
   , page            : Page           -- current page whose contents shall be displayed
   , credentials     : Credentials    -- user's credentials used to log in with
   , source          : Source         -- the selected source for the help pages
   , notes           : List String    -- the current user's list of notes
   , logged          : Maybe Int      -- the user id (Just Int if he is logged or nothing)
   , command         : String         -- the command that the user has typed into the field
   }  




-- INITIALIZATION




init : Flags -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url key =
   let ( model, urlCmd )    =
          urlUpdate url { contentRequest  = None
                        , navKey          = key
                        , page            = Home
                        , credentials     = { email = "" , password = "" }
                        , source          = Cheatsh
                        , notes           = []
                        , logged          = Nothing
                        , command         = ""
                        } 
   in
       ( model, Cmd.batch [ urlCmd] )




-- ACTIONS




type Msg = GotContent     (Result Http.Error String       ) -- response for RequestContent action
         | GotIdOfUser    (Result Http.Error String       ) -- response for LoginUser action
         | GotNotes       (Result Http.Error (List String)) -- response for RequestNotes action
         | Saved          (Result Http.Error ()           ) -- response for Save action
         | ChangeEmail    String                            -- \
         | ChangePass     String                            -- | 
         | ChangeText     String                            -- } change the model accoring to the input
         | ChangeCommand  String                            -- |
         | RequestContent                                   -- /
         | RequestNotes                                     -- send request for the user's saved pages
         | UrlChange      Url                               -- the result of changing the url
         | ClickedLink    UrlRequest                        -- produced when the user clicks on a link
         | LoginUser                                        -- attempt login with the current credentials
         | Discard                                          -- discard the content of the text field
         | Save                                             -- save the content in the text field
         | SelectSource   String                            -- select a source for the search




-- JSON ENCODERS




encodeUserId : Int -> E.Value
encodeUserId n = 
   E.object [ ( "userid", E.int n ) ]

encodeCredentials : String -> String -> E.Value
encodeCredentials username password =
   E.object [ ( "username", E.string username )
            , ( "password", E.string password )
            ]

encodeManPageRequest : String -> String -> E.Value
encodeManPageRequest src cmd =
   E.object [ ( "source" , E.string src )
            , ( "command", E.string cmd )
            ]

encodeText : String -> Int -> E.Value
encodeText t id =
   E.object [ ( "text"  , E.string t  )
            , ( "userid", E.int    id )
            ]




-- HTTP REQUESTS




save : String -> Int -> Cmd Msg
save text id =
   Http.post
      { url    = "http://localhost/saveNote.php"
      , body   = Http.jsonBody <| encodeText text id
      , expect = Http.expectWhatever Saved
      }

getText : String -> String -> Cmd Msg
getText s c =
   Http.post
      { url    = "http://localhost/getContent.php" 
      , body   = Http.jsonBody <| encodeManPageRequest s c
      , expect = Http.expectString GotContent 
      }

login : String -> String -> Cmd Msg
login user pass =
   Http.post
      { url    = "http://localhost/login.php"
      , body   = Http.jsonBody <| encodeCredentials user pass
      , expect = Http.expectString GotIdOfUser 
      }

getNotes : Int -> Cmd Msg
getNotes n =
   Http.post
      { url    = "http://localhost/getNotesByUserId.php"
      , body   = Http.jsonBody <| encodeUserId n
      , expect = Http.expectJson GotNotes (D.list D.string)
      }




-- UPDATE




update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of
      GotContent result -> 
         case result of 
            Ok text -> ({model | contentRequest = Success text}, Cmd.none)
            Err _   -> ({model | contentRequest = Failure}, Cmd.none)
        
      RequestContent -> 
         ({model | contentRequest = None}, getText ( case model.source of
                                                       Cheatsh   -> "cheat.sh"
                                                       ManPages  -> "man"
                                                       TLDRPages -> "tldr"
                                                   ) ( model.command ))

      ClickedLink req ->
         case req of
            Browser.Internal url ->
               ( model, Navigation.pushUrl model.navKey <| Url.toString url )

            Browser.External href ->
               ( model, Navigation.load href )

      UrlChange url ->
         urlUpdate url model

      RequestNotes -> ( model 
                      , case model.logged of 
                           Just x -> getNotes x 
                           Nothing -> Cmd.none
                      )

      Discard -> 
         ( { model | contentRequest = None } , Cmd.none )

      ChangeEmail value -> 
         ( { model | credentials = { email = value
                                   , password = model.credentials.password } } 
           , Cmd.none )

      ChangeCommand value -> 
         ( { model | command = value }, Cmd.none)

      ChangePass value ->
         ( { model | credentials = { email = model.credentials.email, password = value } } , Cmd.none )

      LoginUser -> ( model, login model.credentials.email model.credentials.password )

      Save -> ( model, case model.contentRequest of
                          Success c  -> case model.logged of
                                           Just id -> save c id
                                           Nothing -> Cmd.none
                          _          -> Cmd.none
              )

      ChangeText text -> ( { model | contentRequest = Success text } , Cmd.none )

      SelectSource s -> let src = case s of 
                                     "cheat.sh"   -> Cheatsh
                                     "TLDR Pages" -> TLDRPages
                                     _            -> ManPages
                        in  ({ model | source = src } , Cmd.none) 
      GotIdOfUser id -> 
          case id of 
             Ok i  -> ({ model | logged = String.toInt i }, Cmd.none)
             Err _ -> (model, Cmd.none)
 
      GotNotes notes -> 
          case notes of 
             Ok ns -> ({ model | notes = ns }, Cmd.none)
             Err _ -> ( model, Cmd.none)
      
      Saved _ -> ( model, Cmd.none )
 



-- HELPER FUNCTIONS




urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )

decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser

routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map LandingPage top
        , UrlParser.map Login       (UrlParser.s "login")
        , UrlParser.map Stack       (UrlParser.s "stack")
        , UrlParser.map Home        (UrlParser.s "home")
        ]




-- VIEW




view : Model -> Browser.Document Msg
view model =
   { title = "LMH"
   , body = [ div []
                  [ header model
                  , mainContent model
                  ]
      ]
   }

menu : Model -> Html Msg 
menu model =
   div [] []


mainContent : Model -> Html Msg
mainContent model = 
   Grid.container [] <|
      case model.page of
         LandingPage -> landingpage model
         Home        -> homepage    model 
         Login       -> loginpage   model 
         Stack       -> stackpage   model 
         NotFound    -> notfound    model 

homepage : Model -> List (Html Msg)
homepage model =
   [ inputCard model ]

loginpage : Model -> List (Html Msg)
loginpage model =
   [ loginPrompt model ]

landingpage : Model -> List (Html Msg)
landingpage model = 
   [ div (List.map applyStyle styles)
         [ case model.logged of
              Just _  -> text "Welcome back!"
              Nothing -> text "Log in to see your notes!" 
         ]
   ]


applyStyle t =
   Attr.style (Tuple.first t) (Tuple.second t)

styles : List (String, String)
styles = 
  [ ("font-family", "monospace")
  , ("font-weight", "bold")
  , ("text-align", "center")
  , ("margin-top", "20%")
  , ("font-size", "50px")
  ]

stackpage : Model -> List (Html Msg)
stackpage model =
   [ div [] ( List.map elemToCard model.notes ) ]


elemToCard : String -> Html Msg
elemToCard t =
   textarea [ Attr.class "form-control"
            , Attr.style "height" "20rem"
            , Attr.style "width" "20rem"
            , Attr.style "margin" "auto"
            , Attr.style "margin-top" "2rem"
            , Attr.style "margin-bottom" "2rem"
            ]
            [ text t ]




notfound : Model -> List (Html Msg)
notfound model =
   [ div []
         [ text "NOT FOUND" ]
   ]

inputCard : Model -> Html Msg
inputCard model =
   div [ Attr.class "card text-white bg-dark mb-3"
       , Attr.style "max-width" "31rem"
       , Attr.style "width" "auto"
       , Attr.style "margin" "auto" 
       , Attr.style "top" "5rem"
       ]
       [ div [ Attr.class "card-header" ]
             [ searchbar model 
             , sourceSelection model 
             , spacer 0.3
             ]
 
       , div [ Attr.class "input-group" ]
             [ textarea [ Attr.class "form-control"
                        , Attr.style "height" "30rem" 
                        , value ( case model.contentRequest of Success text -> text
                                                               _            -> "")
                        , onInput ChangeText
                        ]
                        [ textArea model ]
             ]
       
       , div [ Attr.style "text-align" "center" ]
             [ button [ Attr.class "btn btn-primary" 
                      , Attr.style "width" "100px" 
                      , Attr.style "float" "left"
                      , Attr.style "margin" "0.5rem 0rem 0.5rem 7rem"
                      , onClick Save
                      ]
                      [ text "Save" ]
      
             , button [ Attr.class "btn btn-danger" 
                      , Attr.style "width" "100px" 
                      , Attr.style "float" "right" 
                      , Attr.style "margin" "0.5rem 7rem 0.5rem 0rem"
                      , onClick Discard
                      ]
                      [ text "Discard" ]
             ] 
       ]



sourceSelection : Model -> Html Msg
sourceSelection model =
   select [ Attr.class "form-control" 
          , onInput SelectSource
          , Attr.style "margin-left" "1rem"
          , Attr.style "width" "26.5rem" ]
          [ option [ value "cheat.sh" ]
                   [ text "cheat.sh" ] 
          , option [ value "man pages" ]
                   [ text "man pages" ]
          , option [ value "TLDR Pages" ]
                   [ text "TLDR Pages" ]
          ]

bannerImageLink : String
bannerImageLink = 
   "https://static.onecms.io/wp-content/uploads/sites/12/2016/11/GettyImages-533232539-2000.jpg"

loginPrompt : Model -> Html Msg
loginPrompt model = 
   div [ Attr.class "card mb-3"
       , Attr.style "max-width" "40rem"
       , Attr.style "width" "auto"
       , Attr.style "margin" "auto" 
       , Attr.style "top" "5rem"
       ]
   [ img [ Attr.class "card-img-top" 
         , Attr.src bannerImageLink 
         , Attr.style "object-fit" "cover"
         , Attr.style "height" "80px"  ] 
         []
   , div [ Attr.class "card-body" ]
         [ Html.form []
                     [ div [ Attr.class "form-group" ]
                           [ label [] 
                                   [ text "Email address" ]
                           , input [ Attr.class "form-control" 
                                   , value model.credentials.email
                                   , onInput ChangeEmail
                                   , Attr.placeholder "Enter your email" ]
                                   []
                           ]
                     , div [ Attr.class "form-group" ]
                           [ label [] 
                                   [ text "Password" ]
                           , input [ Attr.type_ "password" 
                                   , Attr.class "form-control" 
                                   , value model.credentials.password
                                   , onInput ChangePass
                                   , Attr.placeholder "Password"]
                                   []
                           ]
                     , button [ Attr.type_ "submit"
                              , Attr.class "btn btn-primary"
                              , onClick LoginUser
                              ]
                              [ text "Submit" ]
                    
                     ]
         ]
   ]


header : Model -> Html Msg
header model = 
   nav [ Attr.class "navbar navbar-expand-lg navbar-dark bg-dark"
       , Attr.style "color" "white"
       ]
       [ aBuilder [ "navbar-brand" ] "#" ">little_manual_help"  
       , div [ Attr.class "collapse navbar-collapse" ]
             [ ul [ Attr.class "navbar-nav mr-auto" ]
                  [ li [ Attr.class "nav-item active" ]
                       [ aBuilder [ "nav-link" ] "#home" "Home" ]
                  , li [ Attr.class "nav-item active" ]
                       [ a [ Attr.class "nav-link"
                           , Attr.href  "#stack" 
                           , onClick RequestNotes
                           ]
                           [ text "MyManual" ]
                       ] 
                  , li [ Attr.class "nav-item" ]
                       [ aBuilder [ "nav-link" ] "#login" "Login" ]
                  ]  
             ]
       ]  


aBuilder : List String -> String -> String -> Html Msg
aBuilder cls hrf txt =
   a ((++) (List.map Attr.class cls) 
           <| List.singleton 
           <| Attr.href hrf
     )
     [ text txt ]

      
searchbar : Model -> Html Msg
searchbar model =
   nav [ Attr.class "navbar navbar-dark bg-dark" ]
       [ Html.form [ Attr.class "form-inline" ]
              [ input [ Attr.class "form-control mr-sm-2"
                      , Attr.type_ "search" 
                      , Attr.placeholder "Search"
                      , Attr.style "width" "21rem"
                      , value model.command
                      , onInput ChangeCommand
                      ]
                      []
              , button [ Attr.class "btn btn-secondary" 
                       , onClick RequestContent 
                       , Attr.type_ "submit"
                       ] 
                       [ text "Search" ]
              ]
        ]   
  
     
spacer : Float -> Html Msg
spacer size = div [ Attr.style "height" ((++) (String.fromFloat size) "rem") ] []

textArea model =
   case model.contentRequest of
      Failure ->
         text "please try again"

      None ->
         text ""

      Success fullText ->
         text fullText 



