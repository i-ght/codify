open System
open System.IO
open System.Globalization
open System.Collections.Generic
open System.Text.RegularExpressions

open CsvHelper

type ContentEntryData =
    { Date: DateTimeOffset 
      Name: string
      Tags: string 
      Content: string }

type EntryDay = DateOnly

type Entry =
    { Date: EntryDay 
      Name: string
      Tags: string []
      Content: string }

type EntryMonth = DateOnly
type Entries = ResizeArray<Entry>
type EntryDays = Dictionary<EntryDay, Entries>
type EntriesMap = Dictionary<EntryMonth, EntryDays>

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

    let newLine =
        Environment.NewLine

module Date =
    let ofDateTime (dateTime: DateTimeOffset) =
        let (date, _time, _offset) =
            dateTime.Deconstruct()
        date

module Array =
    let inline lasti (arr:_[]) = arr.Length - 1

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

module Entry =

    let month (day: DateOnly): EntryMonth =
        let days =
            TimeSpan.FromDays(float (day.Day - 1))

        (*Changes date day of month to be one and hour:minute:second of time to be 00:00:00 *)
        let updated =
            day
                .ToDateTime(TimeOnly(0, 0, 0))
                .Subtract(days)
                .ToUniversalTime()
(*                 .Add(day.Offset) *)
        Date.ofDateTime (DateTimeOffset(updated))

    [<AutoOpen>]
    module internal Adoc =
        let tags (tags: string) =
            String.split ',' tags
            |> Array.map String.trim
            
        let writer path =
            new StreamWriter(path=path)

        let appendLine (line: string) (writer: StreamWriter) =
            writer.WriteLine(line)

        let flush (writer: StreamWriter) =
            writer.Flush()

        let splitParagraph (paragraph: string) =
            Regex.Split(paragraph, @"(?<=[.!?])\s+")

        let adocLines (entry: Entry) =
            let lines =
                entry.Content.TrimEnd(
                    Env.newLine.ToCharArray()
                )
            let lines = 
                lines.Split(Env.newLine)
                |> Array.map (fun line -> $"{line} +")
   
            let i = Array.lasti lines
            lines[i] <- lines[i].TrimEnd(" +".ToCharArray())

            List.ofSeq lines

        let appendNameSection (entry: Entry) (writer: StreamWriter) =
            let section =
                [ ""
                  $"==== {entry.Name}" ]
                |> String.concat Env.newLine
            appendLine section writer

        let appendContent (entry: Entry) (writer: StreamWriter) =

            let content =
                String.concat
                <| Env.newLine
                <| ([""] @ adocLines entry)
            
            appendLine content writer

        let header =
            [ "= Memory Map"
              ":toc: left"
              ":toclevels: 3" ]
            |> String.concat Env.newLine

        let appendHeader (writer: StreamWriter) =
            appendLine header writer

        let appendMonthSection (month: EntryMonth) (writer: StreamWriter) =
            let ``yyyy MMM`` = month.ToString("yyyy MMM")
            let section =
                [ ""
                  $"== {``yyyy MMM``}" ]
                |> String.concat Env.newLine

            appendLine section writer

        let appendDaySection (day: EntryDay) (writer: StreamWriter) =
            let date = day.ToString("MM-dd")
            let section =
                [ ""
                  $"=== {date}" ]
                |> String.concat Env.newLine
            appendLine section writer

    let ofData (entries: ContentEntryData seq) = seq {
        for entry in entries do
            yield 
                { Date=Date.ofDateTime entry.Date
                  Name=entry.Name
                  Tags=tags entry.Tags
                  Content=entry.Content }
    }

    let day (entry: Entry) = entry.Date

    let writeadoc (entries: EntriesMap) =
        use writer =
            writer "code/codex.adoc"
        
        appendHeader writer

        for pair in entries do
            let struct (month, days) = 
                (pair.Key, pair.Value)
            
            appendMonthSection month writer

            for pair in days do
                let struct (day, entries) =
                    (pair.Key, pair.Value)
                
                appendDaySection day writer

                for entry in entries do
                    appendNameSection entry writer
                    appendContent entry writer

            flush writer

let head _argv =
    let ``content.csv`` = "./data/content.csv"
    let es =
        Csv.records<ContentEntryData> ``content.csv``
        |> Entry.ofData
        |> List.ofSeq
        |> List.rev

    (* unique combinations of year and month are used as keys for EntriesMap*)
    let months = 
        List.map Entry.day es
        |> List.map Entry.month
        |> List.ofSeq

    let entries =
        EntriesMap(List.length months)

    for month in months do
        if not <| entries.ContainsKey(month) then
            entries[month] <- EntryDays(8)

    for e in es do
        let struct (day, month) =
            (e.Date, Entry.month e.Date)
        if not <| entries[month].ContainsKey(day) then
            entries[month][day] <- Entries(8)

        let entries = entries[month][day]
        entries.Add(e)

    Entry.writeadoc entries

    0

Env.argv ()
|> head
|> Env.exit