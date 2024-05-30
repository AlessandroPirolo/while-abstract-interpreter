# Specify the path to your .fsproj file
$projectPath = "WhileAbstractInterpreter.fsproj"

# Navigate to the directory containing the .fsproj file
Set-Location (Split-Path $projectPath)

# Build the project using dotnet build
dotnet build $projectPath