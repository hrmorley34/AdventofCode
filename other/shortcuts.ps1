function fetch_input { py $PSScriptRoot\fetch_input.py $args }
Set-Alias -Name fi -Value fetch_input 

function leaderboards { py $PSScriptRoot\leaderboards.py $args }
Set-Alias -Name lb -Value leaderboards 
