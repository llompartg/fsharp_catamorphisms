type FileSystemItem =
    | File of File
    | Directory of Directory
and File = {name: string; size: int}
and Directory = {name: string; items: FileSystemItem list }



[<EntryPoint>]
let main argv =

    let myFile = File {name = "MySuperFile"; size = 100}
    let myFile2 = File {name = "MySuperFile2"; size = 88}
    let home = Directory { name = "home"; items = [myFile; myFile2]}
    
    let bigFile = File {name = "This is the biggest file"; size = 500}
    let usbDrive = Directory { name = "USB2.0"; items = [bigFile]}
    
    let media = Directory { name=  "media"; items = [usbDrive]}

    let etc = Directory { name=  "etc"; items = []}

    let root = Directory { name = "/"; items = [home; media; etc]}

    let rec catamorphismSystemDirectory fFile fDirectory item = 
        let recursive = catamorphismSystemDirectory fFile fDirectory
        match item with
        | File f ->
            fFile f
        | Directory item ->
            let results = item.items |> List.map recursive
            fDirectory item results
                

    let calculateSize (item:FileSystemItem): int =
        let fFile (file: File): int =
            printfn "Size of %s is %i" file.name file.size
            file.size

        let fDirectory (directory: Directory) (sizes: int list): int =
            let result = sizes |> List.sum
            printfn "Total size of %s directory is %i" directory.name result
            result

        catamorphismSystemDirectory fFile fDirectory item


    let existsFileWithGivenName (item:FileSystemItem) (name: string): bool =
        let fFile (file: File): bool =
            file.name = name

        let fDirectory (directory: Directory) (results: bool list): bool =
            let result = results |> List.exists (id)
            result

        catamorphismSystemDirectory fFile fDirectory item    

    calculateSize root |> printfn "%i"
    existsFileWithGivenName root "MySuperFile" |> printfn "%A"
    0 // return an integer exit code
