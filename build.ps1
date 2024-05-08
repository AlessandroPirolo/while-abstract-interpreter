param (
    [string]$argument1,
    [string]$argument2,
    [string]$argument3
)

# Specify the path to your .fsproj file
$projectPath = "WhileAbstractInterpreter.fsproj"

# Navigate to the directory containing the .fsproj file
Set-Location (Split-Path $projectPath)

# Build the project using dotnet build
dotnet build $projectPath

# Run the generated executable
$output = & ".\bin\Debug\net7.0\WhileAbstractInterpreter.exe" $argument1 $argument2 $argument3

Write-Output $output