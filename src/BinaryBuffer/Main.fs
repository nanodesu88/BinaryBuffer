namespace BinaryBuffer

open System
open Microsoft.FSharpLu

[<RequireQualifiedAccess>]
module rec Binary = 
  open System.Text
  open Utility
  
  type Buf = {
    buffer: uint8 list
  }
  
  type Buffer = Buf
  
  let isEmpty buf = buf.buffer |> Seq.isEmpty
  let length buf = buf.buffer |> Seq.length
  let getBuffer buf = buf.buffer
  
  type internal Result<'T> = 'T * Buf
  
  let isLittleEndian = BitConverter.IsLittleEndian
  let toBE bytes = if isLittleEndian then bytes |> List.rev else bytes
  let toLE bytes = if isLittleEndian then bytes else bytes |> List.rev
  
  let create = { buffer = [] }
  
  let ofList lst = { buffer = lst }
  let ofArray arr = arr |> List.ofArray |> ofList
  
  let toArray buf = Array.ofList (buf |> getBuffer)
  let toSeq buf = buf |> getBuffer |> Seq.ofList
  let toList buf = buf |> getBuffer
  
  let map fn (v, buf) = (fn v), buf
  
  let skip count buffer = buffer |> toList |> List.skip count |> ofList
  let take count buffer = buffer |> toList |> List.take count |> ofList
  
  let replace ``begin`` length (chunk: Buf) (destination: Buf) =
    let buf1 = destination |> take ``begin``
    let buf2 = destination |> skip (``begin`` + length)
    
    append buf1 chunk |> append <| buf2
  
  let append (buf: Buf) (buf2: Buf) =
    { buf with buffer = buf.buffer @ buf2.buffer }
    
  let prepend (buf: Buf) (buf2: Buf) =
    { buf with buffer = buf2.buffer @ buf.buffer }
  
  let appendSegmentsTo (buf: Buf) (segments: Buf seq) =
    Seq.append (seq [ buf ]) segments |> Seq.fold (fun st1 st2 -> append st1 st2) create 
  
  let writeByte (value: uint8) (buffer: Buf) = { buffer with buffer = buffer.buffer @ [ value ] }
  let writeChar (value: char) = writeByte (uint8 value)

  let writeByteList (value: uint8 list) (buffer: Buf) = { buffer with buffer = buffer.buffer @ value }
  let writeByteSeq seq buffer = seq |> List.ofSeq |> writeByteList <| buffer
  
  let writeUInt8 = writeByte
  let writeUInt8List = writeByteList
  let writeUInt8Array = List.ofArray >> writeUInt8List
  
  let internal writeSegment value buffer =
    value |> writeUInt8List <| buffer
  
  let writeUInt16 (value: uint16) buffer = value |> getBytesFromUInt16 |> writeSegment <| buffer 
  let writeUInt32 (value: uint32) buffer = value |> getBytesFromUInt32 |> writeSegment <| buffer
  let writeUInt64 (value: uint64) buffer = value |> getBytesFromUInt64 |> writeSegment <| buffer
  let writeInt8 (value: int8) buffer = buffer |> writeUInt8 (uint8 value)
  let writeInt16 (value: int16) buffer = value |> getBytesFromInt16 |> writeSegment <| buffer
  let writeInt32 (value: int32) buffer = value |> getBytesFromInt32 |> writeSegment <| buffer
  let writeInt64 (value: int64) buffer = value |> getBytesFromInt64 |> writeSegment <| buffer
  
  let writeUInt16BE (value: uint16) buffer = value |> getBytesFromUInt16 |> toBE |> writeSegment <| buffer 
  let writeUInt32BE (value: uint32) buffer = value |> getBytesFromUInt32 |> toBE |> writeSegment <| buffer
  let writeUInt64BE (value: uint64) buffer = value |> getBytesFromUInt64 |> toBE |> writeSegment <| buffer
  let writeInt16BE (value: int16) buffer = value |> getBytesFromInt16 |> toBE |> writeSegment <| buffer
  let writeInt32BE (value: int32) buffer = value |> getBytesFromInt32 |> toBE |> writeSegment <| buffer
  let writeInt64BE (value: int64) buffer = value |> getBytesFromInt64 |> toBE |> writeSegment <| buffer
  
  let writeUInt16LE (value: uint16) buffer = value |> getBytesFromUInt16 |> toLE |> writeSegment <| buffer 
  let writeUInt32LE (value: uint32) buffer = value |> getBytesFromUInt32 |> toLE |> writeSegment <| buffer
  let writeUInt64LE (value: uint64) buffer = value |> getBytesFromUInt64 |> toLE |> writeSegment <| buffer
  let writeInt16LE (value: int16) buffer = value |> getBytesFromInt16 |> toLE |> writeSegment <| buffer
  let writeInt32LE (value: int32) buffer = value |> getBytesFromInt32 |> toLE |> writeSegment <| buffer
  let writeInt64LE (value: int64) buffer = value |> getBytesFromInt64 |> toLE |> writeSegment <| buffer
  
  let writeBoolean (value: bool) buffer = buffer |> writeUInt8 (if value then 1uy else 0uy)

  let writeSingle = Binary.writeInt16
  let writeDouble (value: double) buffer = value |> getBytesFromDouble |> writeSegment <| buffer
  let writeChars (value: string) (encoding: Encoding) (buffer: Buf) =
    encoding.GetBytes value |> List.ofArray |> writeUInt8List <| buffer
  
  let writeString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeInt32 (value |> String.length) |> writeByteSeq (value |> Encoding.getBytes encoding)
  let writeCString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeByteSeq (value |> Encoding.getBytes encoding) |> writeUInt8 0uy
  
  let readByte (buf: Buf) = buf |> toList |> List.head, buf |> toList |> List.tail |> ofList
  let readChar (buf: Buf) = buf |> readByte |> map char
  
  let readByteList count buf = buf.buffer |> List.take count, buf |> skip count
  let readBuffer count buf = buf |> take count, buf |> skip count
  
  let readUInt8  (buf: Buf) = readByte buf |> map uint8
  let readUInt16 (buf: Buf) = readByteList 2 buf |> map toUInt16
  let readUInt32 (buf: Buf) = readByteList 4 buf |> map toUInt32
  let readUInt64 (buf: Buf) = readByteList 8 buf |> map toUInt64
  let readInt8   (buf: Buf) = readByte buf |> map int8
  let readInt16  (buf: Buf) = readByteList 2 buf |> map toInt16
  let readInt32  (buf: Buf) = readByteList 4 buf |> map toInt32
  let readInt64  (buf: Buf) = readByteList 8 buf |> map toInt64
  
  let readUInt16BE (buf: Buf) = readByteList 2 buf |> map (toBE >> toUInt16)
  let readUInt32BE (buf: Buf) = readByteList 4 buf |> map (toBE >> toUInt32)
  let readUInt64BE (buf: Buf) = readByteList 8 buf |> map (toBE >> toUInt64)
  let readInt16BE  (buf: Buf) = readByteList 2 buf |> map (toBE >> toInt16)
  let readInt32BE  (buf: Buf) = readByteList 4 buf |> map (toBE >> toInt32)
  let readInt64BE  (buf: Buf) = readByteList 8 buf |> map (toBE >> toInt64)
  
  let readUInt16LE (buf: Buf) = readByteList 2 buf |> map (toLE >> toUInt16)
  let readUInt32LE (buf: Buf) = readByteList 4 buf |> map (toLE >> toUInt32)
  let readUInt64LE (buf: Buf) = readByteList 8 buf |> map (toLE >> toUInt64)
  let readInt16LE  (buf: Buf) = readByteList 2 buf |> map (toLE >> toInt16)
  let readInt32LE  (buf: Buf) = readByteList 4 buf |> map (toLE >> toInt32)
  let readInt64LE  (buf: Buf) = readByteList 8 buf |> map (toLE >> toInt64)
  
  let readUInt8List = readByteList
  
  let readSingle (buf: Buf) = readUInt8List 4 buf |> map toSingle
  let readDouble (buf: Buf) = readUInt8List 8 buf |> map toDouble
  let readBoolean (buf: Buf) = readUInt8 buf |> map ((=) 0uy >> not)
  
  let readString (encoding: Encoding) (buf: Buf) =
    buf
    |> readInt32
    ||> readUInt8List
    |> map (Array.ofList >> Encoding.getString encoding)
  
  let readCString (encoding: Encoding) (buf: Buf) =
    Seq.findIndex ((=) 0uy) buf.buffer
    |> fun idx -> buf |> take idx |> toArray |> Encoding.getString encoding, buf |> skip (1 + idx)
  
  let stringifyList (buffer: uint8 list) =
    buffer |> Seq.map(fun b -> b.ToString "x2") |> Text.join " "
  
  let stringify (buffer: Buf) = buffer |> getBuffer |> stringifyList
  