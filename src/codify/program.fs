open System
open System.IO
open System.Globalization
open System.Collections.Generic

open CsvHelper

type ContentEntryData =
    { Date: DateTimeOffset 
      Name: string
      Tags: string 
      Data: string }

type ContentEntry =
    { Date: DateTimeOffset 
      Name: string
      Tags: string []
      Data: string }

module Env =

    let argv () =
        Environment.GetCommandLineArgs()

    let exit code =
        Environment.Exit(code)

    let var name =
        let envVar = Environment.GetEnvironmentVariable(name)
        if envVar |> isNull then
            ValueNone
        else 
            ValueSome envVar

module String =
    let split (c: char) (s: string) = s.Split(c)
    
    let trim (s: string) = s.Trim()
    

module Csv =

    let records<'a> (path: string) =
        use reader =
            new StreamReader(
                path,
                detectEncodingFromByteOrderMarks=true
            )
        use csv =
            new CsvReader(
                reader,
                CultureInfo.InvariantCulture
            )
        let records =
            csv.GetRecords<'a>()

        records |> List.ofSeq

let tags (tags: string) =
    String.split ',' tags
    |> Array.map String.trim

let contentEntries (data: ContentEntryData seq) = seq {
    for entryData in data do
        let entry =
            { Date=entryData.Date
              Name=entryData.Name
              Tags=tags entryData.Tags
              Data=entryData.Data }
        yield entry
}

let asciiDocFileName (date: DateTimeOffset) =
    let month =
        date
            .ToString("MMM")
            .ToLower()
    $"content/{date.Year}{month}.adoc"

let entryWriter (date: DateTimeOffset) =
    let writer =
        new StreamWriter(
            path=asciiDocFileName date,
            append=true
        )
    writer

let appendDocHeader (writer: StreamWriter) (date: DateTimeOffset) =
    let month = date.ToString("MMMM")
    writer.WriteLine($"= {date.Year} {month}")

let appendPoem (writer: StreamWriter) (entry: ContentEntry) =
    let date = entry.Date.ToString("yyyy-MM-dd")
    writer.WriteLine()
    writer.WriteLine($"== {date}")
    writer.WriteLine()
    writer.WriteLine($"=== {entry.Name}")
    writer.WriteLine()
    
    let lines = entry.Data.Split(Environment.NewLine)
    for line in lines do
        if not <| String.IsNullOrWhiteSpace(line) then
            writer.WriteLine($"{line} +")
let computeEntry (entry: ContentEntry) =
    use writer = entryWriter entry.Date
    let info =
        FileInfo(asciiDocFileName entry.Date)
    if info.Length = 0 then
        appendDocHeader writer entry.Date
    match entry.Tags[0] with
    | "Poem" -> appendPoem writer entry
    | _ -> ()
    

let head _argv =
    let ``content.csv`` = "./data/content.csv"
    let entries =
        Csv.records<ContentEntryData> ``content.csv``
        |> contentEntries

    for entry in entries do
        computeEntry entry

    0

Env.argv ()
|> head
|> Env.exit