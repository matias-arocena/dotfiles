$testchoco = choco -v
if(-not($testchoco)){
    $IsAdmin = (New-Object System.Security.Principal.WindowsPrincipal([System.Security.Principal.WindowsIdentity]::GetCurrent())).IsInRole([System.Security.Principal.WindowsBuiltInRole]::Administrator)
    if (-not($IsAdmin)){
        Write-Error "You need to run as an administrator to install the environment"
        exit 1
    }

    Write-Output "Installing new system"

    Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    Set-ExecutionPolicy Bypass -Scope Process -Force; Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

    choco install sudo -y
    choco install git -y

    Function DotFilesSetup {
        git --git-dir=$HOME/.cfg/ --work-tree=$HOME $args
    }
    Set-Alias -Name dot -Value DotFilesSetup

    Write-Output ".cfg" >> ${HOME}/.gitignore

    git clone --branch windows --bare "git@github.com:matias-arocena/dotfiles.git" $HOME/.cfg
    dot checkout
    dot config --local status.showUntrackedFiles no

    choco install nodejs-lts -y
    choco install ctags -y
    choco install python3 -y
    pip3 install ue4cli
    pip3 install ctags-ue4cli
    pip3 install neovim

    choco install neovim -y
    Invoke-WebRequest -useb https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim | New-Item "$(@($env:XDG_DATA_HOME, $env:LOCALAPPDATA)[$null -eq $env:XDG_DATA_HOME])/nvim-data/site/autoload/plug.vim" -Force

} else {
    Function DotFilesSetup {
        git --git-dir=$HOME/.cfg/ --work-tree=$HOME $args
    }
    Set-Alias -Name dot -Value DotFilesSetup
}

$Env:XDG_CONFIG_HOME = "$HOME\.config"
$Env:GIT_EDITOR = "nvim"
$Env:MYVIMRC = "$HOME\.config\nvim\init.vim"
