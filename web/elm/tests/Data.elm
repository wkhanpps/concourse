module Data exposing
    ( check
    , dashboardPipeline
    , elementPosition
    , httpInternalServerError
    , httpNotFound
    , httpNotImplemented
    , httpUnauthorized
    , input
    , job
    , jobBuild
    , jobId
    , jobName
    , pipeline
    , pipelineName
    , resource
    , resourceName
    , teamName
    , version
    , versionedResource
    , withArchived
    , withDisableManualTrigger
    , withFinishedBuild
    , withGroups
    , withInputs
    , withName
    , withNextBuild
    , withPaused
    , withPipelineName
    , withPublic
    , withTeamName
    , withTransitionBuild
    )

import Browser.Dom
import Concourse
import Concourse.BuildStatus as BuildStatus
import Dashboard.Group.Models
import Dict exposing (Dict)
import Http
import Time


httpUnauthorized : Result Http.Error a
httpUnauthorized =
    Err <|
        Http.BadStatus
            { url = "http://example.com"
            , status =
                { code = 401
                , message = ""
                }
            , headers = Dict.empty
            , body = ""
            }


httpNotFound : Result Http.Error a
httpNotFound =
    Err <|
        Http.BadStatus
            { url = "http://example.com"
            , status =
                { code = 404
                , message = "not found"
                }
            , headers = Dict.empty
            , body = ""
            }


httpNotImplemented : Result Http.Error a
httpNotImplemented =
    Err <|
        Http.BadStatus
            { url = "http://example.com"
            , status =
                { code = 501
                , message = "not implemented"
                }
            , headers = Dict.empty
            , body = ""
            }


httpInternalServerError : Result Http.Error a
httpInternalServerError =
    Err <|
        Http.BadStatus
            { url = "http://example.com"
            , status =
                { code = 500
                , message = "internal server error"
                }
            , headers = Dict.empty
            , body = ""
            }


check : Concourse.CheckStatus -> Concourse.Check
check status =
    case status of
        Concourse.Started ->
            { id = 0
            , status = Concourse.Started
            , createTime = Just <| Time.millisToPosix 0
            , startTime = Just <| Time.millisToPosix 0
            , endTime = Nothing
            , checkError = Nothing
            }

        Concourse.Succeeded ->
            { id = 0
            , status = Concourse.Succeeded
            , createTime = Just <| Time.millisToPosix 0
            , startTime = Just <| Time.millisToPosix 0
            , endTime = Just <| Time.millisToPosix 1000
            , checkError = Nothing
            }

        Concourse.Errored ->
            { id = 0
            , status = Concourse.Errored
            , createTime = Just <| Time.millisToPosix 0
            , startTime = Just <| Time.millisToPosix 0
            , endTime = Just <| Time.millisToPosix 1000
            , checkError = Just "something broke"
            }


resource : String -> Concourse.Resource
resource pinnedVersion =
    { teamName = teamName
    , pipelineName = pipelineName
    , name = resourceName
    , failingToCheck = False
    , checkError = ""
    , checkSetupError = ""
    , lastChecked = Nothing
    , pinnedVersion = Just <| version pinnedVersion
    , pinnedInConfig = False
    , pinComment = Nothing
    , icon = Nothing
    }


pipeline : String -> Int -> Concourse.Pipeline
pipeline team id =
    { id = id
    , name = "pipeline-" ++ String.fromInt id
    , paused = False
    , archived = False
    , public = True
    , teamName = team
    , groups = []
    }


dashboardPipeline : Int -> Bool -> Dashboard.Group.Models.Pipeline
dashboardPipeline id public =
    { id = id
    , name = pipelineName
    , teamName = teamName
    , public = public
    , isToggleLoading = False
    , isVisibilityLoading = False
    , paused = False
    , archived = False
    , stale = False
    , jobsDisabled = False
    }


withPaused : Bool -> { r | paused : Bool } -> { r | paused : Bool }
withPaused paused p =
    { p | paused = paused }


withArchived : Bool -> { r | archived : Bool } -> { r | archived : Bool }
withArchived archived p =
    { p | archived = archived }


withPublic : Bool -> { r | public : Bool } -> { r | public : Bool }
withPublic public p =
    { p | public = public }


withName : String -> { r | name : String } -> { r | name : String }
withName name p =
    { p | name = name }


withGroups : List Concourse.PipelineGroup -> { r | groups : List Concourse.PipelineGroup } -> { r | groups : List Concourse.PipelineGroup }
withGroups groups p =
    { p | groups = groups }


job : Int -> Int -> Concourse.Job
job jobID pipelineID =
    { id = jobID
    , name = jobName
    , pipelineName = "pipeline-" ++ String.fromInt pipelineID
    , teamName = teamName
    , nextBuild = Nothing
    , finishedBuild = Nothing
    , transitionBuild = Nothing
    , paused = False
    , disableManualTrigger = False
    , inputs = []
    , outputs = []
    , groups = []
    }


withNextBuild :
    Maybe Concourse.Build
    -> { r | nextBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
    -> { r | nextBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
withNextBuild build j =
    { j | nextBuild = build |> Maybe.map (updateJobIdentifier j) }


withFinishedBuild :
    Maybe Concourse.Build
    -> { r | finishedBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
    -> { r | finishedBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
withFinishedBuild build j =
    { j | finishedBuild = build |> Maybe.map (updateJobIdentifier j) }


withTransitionBuild :
    Maybe Concourse.Build
    -> { r | transitionBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
    -> { r | transitionBuild : Maybe Concourse.Build, name : String, pipelineName : String, teamName : String }
withTransitionBuild build j =
    { j | transitionBuild = build |> Maybe.map (updateJobIdentifier j) }


updateJobIdentifier :
    { r | name : String, pipelineName : String, teamName : String }
    -> Concourse.Build
    -> Concourse.Build
updateJobIdentifier j b =
    { b
        | job =
            Just
                { jobName = j.name
                , pipelineName = j.pipelineName
                , teamName = j.teamName
                }
    }


withInputs : List Concourse.JobInput -> { r | inputs : List Concourse.JobInput } -> { r | inputs : List Concourse.JobInput }
withInputs inputs j =
    { j | inputs = inputs }


withDisableManualTrigger : Bool -> { r | disableManualTrigger : Bool } -> { r | disableManualTrigger : Bool }
withDisableManualTrigger t j =
    { j | disableManualTrigger = t }


withPipelineName : String -> { r | pipelineName : String } -> { r | pipelineName : String }
withPipelineName name j =
    { j | pipelineName = name }


withTeamName : String -> { r | teamName : String } -> { r | teamName : String }
withTeamName name j =
    { j | teamName = name }


input : List String -> Concourse.JobInput
input passed =
    { name = "input"
    , resource = "res0"
    , passed = passed
    , trigger = True
    }


jobName =
    "job"


teamName =
    "team"


pipelineName =
    "pipeline"


resourceName =
    "resource"


versionedResource : String -> Int -> Concourse.VersionedResource
versionedResource v id =
    { id = id
    , version = version v
    , metadata = []
    , enabled = True
    }


version : String -> Dict String String
version v =
    Dict.fromList [ ( "version", v ) ]


jobId : Concourse.JobIdentifier
jobId =
    { teamName = "t"
    , pipelineName = "p"
    , jobName = "j"
    }


jobBuild : BuildStatus.BuildStatus -> Concourse.Build
jobBuild status =
    { id = 1
    , name = "1"
    , job = Just jobId
    , status = status
    , duration =
        { startedAt =
            case status of
                BuildStatus.BuildStatusPending ->
                    Nothing

                _ ->
                    Just <| Time.millisToPosix 0
        , finishedAt =
            if BuildStatus.isRunning status then
                Nothing

            else
                Just <| Time.millisToPosix 0
        }
    , reapTime = Nothing
    }


elementPosition : Browser.Dom.Element
elementPosition =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { width = 0
        , height = 0
        , x = 0
        , y = 0
        }
    , element =
        { x = 0
        , y = 0
        , width = 1
        , height = 1
        }
    }
