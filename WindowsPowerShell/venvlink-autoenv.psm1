# Copyright 2018 Nick Cox
# Copyright 2020 Niko Pasanen
#
# About this file
# ===============
# This file has been forked from ps-autovent v.0.5.0 by Nick Cox
#     https://github.com/nickcox/ps-autoenv
# 
# Added since:
#   * Modify for venvlink purposes (check for ./venv/   v file)
#   * Check all the parent folders for virtual environments
#   * Change logic: no action on every change in pwd, but only when there
#        is pwd change to project folder from outside project folder, or 
#        vice versa.
#   * Also check the initial folder.
#   * For authorization, check also hash of the file 

Set-StrictMode -Version latest

# Keeps track of current ("old") directory.
$script:currentDir = $pwd

# If pwd inside project directory (or subfolder),
# this is non-null and points to the project folder root.
# Both objects below are always System.IO.DirectoryInfo or null.
$script:currentProjectDir = $null
$script:lastfoundProjectDir = $null

$global:venvlink_autoenv = New-Object PSObject -Property ([ordered]@{
  AUTH_FILE = '~/venvlink-autoenv-auth'
  ENV_FILENAME = 'venvlink-autoenv.ps1'
  ENV_LEAVE_FILENAME = 'venvlink-autoenv.leave.ps1'
  ENABLE_LEAVE = $true
  ASSUME_YES = $false
})


function AuthorizeFile($filePath) {
  if (-not (Test-Path $venvlink_autoenv.AUTH_FILE)) {
    New-Item $venvlink_autoenv.AUTH_FILE
  }

  $content = Get-Content $filePath -Raw
  $hash = (Get-FileHash $filePath  -Algorithm MD5).Hash

  $authline = "$filePath $hash"
  if ((Get-Content $venvlink_autoenv.AUTH_FILE) -contains $authline) {
    return $true
  }

  Write-Warning 'venvlink-autoenv wants to authorize the following script:'
  Write-Host ('=' * 60) -ForegroundColor Red
  Write-Host $content -ForegroundColor Green
  Write-Host ('=' * 60) -ForegroundColor Red

  if ($venvlink_autoenv.ASSUME_YES -eq $true) {
    Write-Host "$([char]0x2713) Auto authorized `n" -ForegroundColor DarkYellow
    $authline >> $venvlink_autoenv.AUTH_FILE
    return $true
  }

  switch (Read-Host "Authorize file ($filePath) ( y / n )") {
    "y" {
      $authline >> $venvlink_autoenv.AUTH_FILE
      return $true
    }
    Default {
      return $false
    }
  }
}

function RunScript {
  [CmdletBinding()]
  param ($scriptFile)

  $scriptFile = Get-Item $scriptFile
  $scriptDir = $scriptFile.Directory 
  if (AuthorizeFile $scriptFile.FullName) {
    Write-Verbose "Running script: $scriptFile"
    
    #Set $PSScriptRoot for convenience
    $block = "param (`$PSScriptRoot)`n" 
    #Give ./venv/venvlink-autoenv.ps1 access to
    # $workdir. This is needed so that 
    # 1) virtual environment can be activated by cd'ing
    #    into any subdirectory if project root 
    # 2) Hardcoding directories in venvlink-autoenv
    #    files is not needed -> Projects can be relocated.
    $block += "`$workdir = '$scriptDir'`n"
    $block += (Get-Content $scriptFile.FullName -Raw)
    
    $output = Invoke-Command `
      -ScriptBlock ([scriptblock]::Create(($block))) `
      -ArgumentList $scriptFile.DirectoryName
  }
}

function LeaveProjectDir {
  [CmdletBinding()]
  
  param ($project_dir)

  Write-Verbose "Leaving project directory '$project_dir'"

  if (-not $script:currentProjectDir) {
      return 
  }
  if (
    $venvlink_autoenv.ENABLE_LEAVE -and (
    $leaveFile = GetVenvlinkFile $project_dir $venvlink_autoenv.ENV_LEAVE_FILENAME)) {
    RunScript $leaveFile
    $script:currentProjectDir = $null
  }
}

function EnterProjectDir {
  [CmdletBinding()]
  param ($project_dir)

  Write-Verbose "Entered project directory '$project_dir'"
  if ($enterFile = GetVenvlinkFile $project_dir $venvlink_autoenv.ENV_FILENAME) {
    RunScript $enterFile
    $script:currentProjectDir = $project_dir
  }
}



function GetVenvlinkFile($Dir, [string]$filename) {
    
    try {
        $venv = Join-Path -Path (Get-Item $Dir -Force) -ChildPath 'venv'
        $file = Join-Path -Path $venv -ChildPath $filename
            if (-not (Test-Path $file)) {
                $file = $false 
            } 
    } catch {
        $file = $false
    }

    return $file
}



function IsProjectDir {
    <#
    .SYNOPSIS
        Check if a directory is a project directory

    .DESCRIPTION
        Check if a directory is a venvlink project directory.
        A venvlink project directory has file ./venv/ENV_FILENAME

        This function returns true or false.
        This function modifies $script:lastfoundProjectDir
    #>
    [CmdletBinding()]
    param (
        # The directory to be checked.
        $Dir
    )   

    Write-Verbose "Checking if $Dir is a project directory"
    $venvlink_env = GetVenvlinkFile $Dir $venvlink_autoenv.ENV_FILENAME
    if ($venvlink_env -eq $false)
    {
        $ret = $false     
    } else {
        $ret = $true   
        $script:lastfoundProjectDir = (Get-Item (Get-Item $venvlink_env).Directory.parent.FullName)
    }
    
    return $ret 
  }

  
  function InProject {
    <#
    .SYNOPSIS
        Check if a directory is within a project directory

    .DESCRIPTION
        Check if a directory or one of it's subdirectories is a 
        venvlink project directory.  A venvlink project directory has
        file ./venv/venvlink-autoenv.ps1

        This function returns true or false.
        This function modifies $script:lastfoundProjectDir
    #>
    [CmdletBinding()]
    param (
        # The directory to be checked.
        $Dir,
        # Just for verbose output
        [bool]$suppressverbose
    )   

    if (!($suppressverbose))    
    {
        Write-Verbose "Checking if $Dir is a project directory or one of it's subfolders"
    }
    
    $project_found = IsProjectDir $Dir
    if (-not $project_found) {
        # -Force is needed since some special directories such as 
        # "C:\Users\All Users" do not have a parent.
        if ((Get-Item $Dir -Force).parent) {
            $project_found = InProject (Get-Item $Dir -Force).parent.FullName $true
        }
    }   
    
    return $project_found
    
  }


  function AutoEnv {
    [CmdletBinding()]
    param (
        # The directory that user is entering
        $newDir
    )   

    Write-Verbose "Running AutoEnv, currentDir: $currentDir, newDir: $newDir, currentProjectDir:$script:currentProjectDir, lastfoundProjectDir:$script:lastfoundProjectDir"

    try {
      if ($newDir.Path -eq $currentDir.Path) {
        return
      }
      
      $current_in = InProject $currentDir
      $new_in = InProject $newDir
      Write-Verbose "Current folder ($currentDir) in a project: $current_in"
      Write-Verbose "New folder ($newDir) in a project: $new_in"
      Write-Verbose "currentProjectDir:$script:currentProjectDir, lastfoundProjectDir:$script:lastfoundProjectDir"

      if ($current_in -eq $new_in) {
            # If current and new directory are both
            # inside or outside the project folder, there
            # is no need to run anything.
            
            # But there is one exception which must be checked:
            # User changes directly from one project to another
            if ($script:lastfoundProjectDir -and $script:currentProjectDir) {
                $samefolder = ($script:lastfoundProjectDir.FullName -eq  $script:currentProjectDir.FullName)
            } else {
                # dirs can be null (e.g. after deactivation).
                $samefolder = $false 
            }

            if (($current_in -and $new_in) -and (-not $samefolder)) {
                # User changed directly from one project to another.
                LeaveProjectDir $script:currentProjectDir
                EnterProjectDir $script:lastfoundProjectDir

            }
      } elseif ($new_in) {
            # newDir is inside project
            # currentDir is outside project
            # -> Just entered project folder
            EnterProjectDir $script:lastfoundProjectDir
          
       } elseif ($current_in) {
            # newDir is outside project
            # currentDir is inside project
            # -> Just left project folder
            LeaveProjectDir $script:currentProjectDir
      }

      $script:currentDir = $newDir
      return 
    }
    catch {
      Write-Warning "Could not execute venvlink_autoenv script. `n$_.Exception.Message"
    }
  }

  
# Add validator to PWD which is called every time directory is changed.
# The validator checks if shell has left or entered a project folder.
# DEBUG TIP: Use "AutoEnv $_ -Verbose" instead of "AutoEnv $_"
$validateAttr = (new-object ValidateScript { AutoEnv $_; return $true })
(Get-Variable PWD).Attributes.Add($validateAttr)

$MyInvocation.MyCommand.ScriptBlock.Module.OnRemove = {
  $null = (Get-Variable pwd).attributes.Remove($validateAttr)
  $global:venvlink_autoenv = $null
}

# This is only ran when this module is imported
# Needed for the case, when powershell is launched in a folder that has environment to 
# be activated.
if (InProject $pwd) {
    EnterProjectDir $script:lastfoundProjectDir
}

Export-ModuleMember -Variable $venvlink_autoenv
