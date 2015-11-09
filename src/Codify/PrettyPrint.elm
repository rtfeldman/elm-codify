module Codify.PrettyPrint (typeAlias, typeToString) where

import Codify exposing (Type(..))

import String
import Dict exposing (Dict)


typeAlias : String -> Type -> String
typeAlias str typeValue =
    "type alias " ++ str ++ " =\n" ++ (typeToString False typeValue)


encoder : String -> Type -> String
encoder name typeValue =
    let
        annotation =
            [ name, ":", typeToString False typeValue, "->", "Value" ]
                |> String.join " "

        declaration =
            name ++ " ="

        body =
            toEncoderString False typeValue
    in
        [ annotation, declaration, (indent body) ]
            |> String.join "\n"


{-| Example: ( "name", StringType ) ---> "( \"name\", .name >> string )" -}
fieldPairToEncoderString : ( String, Type ) -> String
fieldPairToEncoderString ( field, typeValue ) =
    String.join " "
        [ "("
        , "\"" ++ field ++ "\","
        , "." ++ field
        , ">>"
        , (toEncoderString True typeValue)
        , ")"
        ]


toEncoderString : Bool -> Type -> String
toEncoderString needsParens typeValue =
    withParens needsParens
        <| case typeValue of
            AliasType aliasName ->
                ( False, "<< TODO handle alias >>" )

            IntType ->
                ( False, "int" )

            FloatType ->
                ( False, "float" )

            BoolType ->
                ( False, "bool" )

            StringType ->
                ( False, "string" )

            MaybeType typeParam ->
                ( True, "Maybe.map " ++ (toEncoderString True typeParam) ++ " >> Maybe.withDefault null" )

            ListType typeParam ->
                ( True, "list << (List.map " ++ (toEncoderString True typeParam) ++ ")")

            RecordType typeDict ->
                if Dict.isEmpty typeDict then
                    ( True, "object []" )
                else
                    let
                        body =
                            typeDict
                                |> Dict.toList
                                |> List.map fieldPairToEncoderString
                                |> multiline "[" "," "]"
                    in
                        ( True, "record\n" ++ (indent body) )

            TupleType typeParams ->
                if List.isEmpty typeParams then
                    ( True, "list []" )
                else
                    let
                        body =
                            typeParams
                                |> List.map (nestedTypeToString (toEncoderString False))
                                |> multiline "[" "," "]"
                    in
                        ( True, "list\n" ++ (indent body) )


decoder : String -> Type -> String
decoder str typeValue =
    "type alias " ++ str ++ " =\n" ++ (typeToString False typeValue)


fieldPairToString : (String, Type) -> String
fieldPairToString (field, fieldType) =
    field ++ " :" ++ (nestedTypeToString (typeToString False) fieldType)


nestedTypeToString : (Type -> String) -> Type -> String
nestedTypeToString converter nestedType =
    let
        converted =
            converter nestedType
    in
        case nestedType of
            RecordType _ ->
                "\n" ++ (indent converted)

            TupleType _ ->
                "\n" ++ (indent converted)

            _ ->
                " " ++ converted


indentSpaces : Int
indentSpaces =
    4


indentation : String
indentation =
    String.repeat indentSpaces " "


indent : String -> String
indent str =
    str
        |> String.split "\n"
        |> List.map (\line -> indentation ++ line)
        |> String.join "\n"


multiline : String -> String -> String -> List String -> String
multiline openingDelimiter elemDelimiter closingDelimiter lines =
    let
        header =
            indentation ++ openingDelimiter ++ " "

        body =
            String.join ("\n" ++ indentation ++ elemDelimiter ++ " ") lines

        footer =
            "\n" ++ indentation ++ closingDelimiter
    in
        header ++ body ++ footer


withParens : Bool -> ( Bool, String ) -> String
withParens needsParens ( supportsParens, baseResult ) =
    if needsParens && supportsParens then
        "(" ++ baseResult ++ ")"
    else
        baseResult

typeToString : Bool -> Type -> String
typeToString needsParens typeValue =
    withParens needsParens
        <| case typeValue of
            AliasType aliasName ->
                ( False, aliasName )

            IntType ->
                ( False, "Int" )

            FloatType ->
                ( False, "Float" )

            BoolType ->
                ( False, "Bool" )

            StringType ->
                ( False, "String" )

            ListType typeParam ->
                ( True, "List " ++ (typeToString True typeParam) )

            MaybeType typeParam ->
                ( True, "Maybe " ++ (typeToString True typeParam) )

            RecordType typeDict ->
                if Dict.isEmpty typeDict then
                    ( False, "{}" )
                else
                    let
                        str =
                            typeDict
                                |> Dict.toList
                                |> List.map fieldPairToString
                                |> multiline "{" "," "}"
                    in
                        ( True, str )

            TupleType typeParams ->
                if List.isEmpty typeParams then
                    ( False, "()" )
                else
                    let
                        str =
                            typeParams
                                |> List.map (nestedTypeToString (typeToString False))
                                |> multiline "(" "," ")"
                    in
                        ( True, str )
