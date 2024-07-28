param (
    [string]$directory,
    [string]$extension,
    [string]$command
)

# Ensure the extension starts with a dot
if ($extension -notmatch '^\.') {
    $extension = ".$extension"
}

# Get all files with the specified extension in the given directory and its subdirectories
$files = Get-ChildItem -Path $directory -Recurse -Filter *$extension

foreach ($file in $files) {
    # Construct the full command with the file path as an argument
    $fullCommand = "$command `"$($file.FullName)`""
    
    echo $file.FullName
    # Execute the command
    Invoke-Expression $fullCommand
}
