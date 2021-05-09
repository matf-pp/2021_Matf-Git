module LeGit (
    Init.init,
    Set.setUserName, Set.setEmail, 
    Set.addIgnore, Set.removeIgnore,
    Set.addRef, Set.addTag,
    Show.showInfo, Show.showIgnores, Show.showHead,
    Action.commit,  
    Action.visitRef, Action.visitTag, Action.visitSha,
    Action.garbageCollector
) where

import qualified LeGit.Init as Init
import qualified LeGit.Set as Set
import qualified LeGit.Show as Show
import qualified LeGit.Action as Action
