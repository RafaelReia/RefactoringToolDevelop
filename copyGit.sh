rsync -azvhW --exclude compiled* --exclude '*.rkt~' --dry-run ~/share/racket/collects/RefactoringTool/  ~/Documents/RefactoringToolDevelop/
rsync -azvhW --exclude .git* --exclude README.* --dry-run  ~/Documents/RefactoringToolDevelop/ ~/share/racket/collects/RefactoringTool/
