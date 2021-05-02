module LeGit (
    LeGit.Init.init,
    LeGit.Set.setUserName, LeGit.Set.setEmail, LeGit.Set.addIgnore, LeGit.Set.removeIgnore,
    LeGit.Show.showInfo, LeGit.Show.showIgnores,
    LeGit.Action.commit,  LeGit.Action.visitRef, LeGit.Action.visitTag, LeGit.Action.visitSha
) where

import LeGit.Init
import LeGit.Set
import LeGit.Show
import LeGit.Action
