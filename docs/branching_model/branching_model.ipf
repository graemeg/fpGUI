.* Created by Graeme Geldenhuys <graemeg@gmail.com> - 2013-04-08
.* This document is encoded as IBM code page 850.
.*
:userdoc.
:docprof toc=123.
:title.fpGUI Branching Model

:h1.fpGUI branching model
:p.In this post I present the development model that I've introduced for all of
my projects (both at work and private) about a year ago, and which has turned
out to be very successful. I've been meaning to write about it for a while now,
but I've never really found the time to do so thoroughly, until now. I won't
talk about any of the projects' details, merely about the branching strategy
and release management.

:cgraphic.
                           :hp2.Time increases to the right:ehp2.

             0.1     0.2                             1.0             1.5
   master  ÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍ>
               \Ä,   /                               /               /
               |  \ /                               /               /
 hotfixes  ÄÄÄÄ|ÄÄÄoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ/ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ/ÄÄÄÄÄÄÄÄÄÄÄ>
               |    \                             /               /
               |     \                           /               /
  release  ÄÄÄÄ|ÄÄÄÄÄÄ\ÄÄÄÄÄÄÄÄÄÄÄÄÄoÍÍÍoÍÍÍoÍÍÍoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄoÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>
 branches      |       \           /     \       \             / \
                \       \         /       \       \           /   \
  develop  ÍÍÍÍÍÍoÍÍÍoÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍoÍÍÍoÍÍÍÍÍoÍÍÍoÍÍÍÍÍÍÍ>
                  \Ä,           /           \           /   /
                  |  \         /             \         /   /
 feature1  ÄÄÄÄÄÄÄ|ÄÄÄoÍÍÍoÍÍÍoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄoÍÍÍoÍÍÍoÄÄÄ/ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>
                  |                                      /
                   \                                    /
 feature2  ÄÄÄÄÄÄÄÄÄoÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>



   :hp2.Key::ehp2.
     ÍÍÍ  and diagonal lines represent active development
     ÄÄÄ  represents no development, just time that has passed
      o   is a commit in the repository
     0.1  Numbers above the master branch is release tags
:ecgraphic.

:p.
Here is an :link reftype=hd res=123.alternative overview graph:elink., which
hides the "time has passed" lines.

:p.
It focuses around Git [http://git-scm.com/] as the tool for the versioning of all of our source code.

.* -------------------------------------
:h2 res=123 hide.Alternative overview graph
:cgraphic.
                           :hp2.Time increases to the right:ehp2.

             0.1     0.2                             1.0             1.5
   master  ÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍ>
               \Ä,   /                               /               /
               |  \ /                               /               /
 hotfixes      |   o                               /               /
               |    \                             /               /
               |     \                           /               /
  release      |      \             oÍÍÍoÍÍÍoÍÍÍo               o
 branches      |       \           /     \       \             / \
                \       \         /       \       \           /   \
  develop  ÍÍÍÍÍÍoÍÍÍoÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍoÍÍÍÍÍoÍÍÍoÍÍÍÍÍoÍÍÍoÍÍÍÍÍÍÍ>
                  \Ä,           /           \           /   /
                  |  \         /             \         /   /
 feature1         |   oÍÍÍoÍÍÍo               oÍÍÍoÍÍÍo   /
                  |                                      /
                   \                                    /
 feature2           oÍÍÍoÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍoÍÍÍÍÍÍÍÍÍÍo



   :hp2.Key::ehp2.
     ÍÍÍ  and diagonal lines represent active development
      o   is a commit in the repository
     0.1  Numbers above the master branch is release tags
:ecgraphic.

.* -------------------------------------
:h2.Why Git?
:p.
For a thorough discussion on the pros and cons of Git compared to centralized 
source code control systems, see [http://whygitisbetterthanx.com/] the web [http://git.or.cz/gitwiki/GitSvnComparsion]. There are plenty of flame wars going 
on there. As a developer, I prefer Git above all other tools around today. Git 
really changed the way developers think of merging and branching. From the 
classic CVS/Subversion world I came from, merging/branching has always been 
considered a bit scary ("beware of merge conflicts, they bite you!") and 
something you only do every once in a while.

:p.
But with Git, these actions are extremely cheap and simple, and they are 
considered one of the core parts of your daily workflow, really. For example, 
in CVS/Subversion books, branching and merging is first discussed in the later 
chapters (for advanced users), while in every Git book, it's already covered 
in chapter 3 (basics).

:p.
As a consequence of its simplicity and repetitive nature, branching and merging 
are no longer something to be afraid of. Version control tools are supposed to 
assist in branching/merging more than anything else.

:p.
Enough about the tools, let's head onto the development model. The model that 
I'm going to present here is essentially no more than a set of procedures that 
every team member has to follow in order to come to a managed software 
development process.

.* -------------------------------------
:h2.Decentralised but centralised
:p.
The repository setup that we use and that works well with this branching model, 
is that with a central "truth" repo. Note that this repo is only considered to 
be the central one (since Git is a DVCS, there is no such thing as a central 
repo at a technical level). We will refer to this repo as origin, since this 
name is familiar to all Git users.

.* :artwork align=center name='centr-decentr.bmp'.
:cgraphic.
   ÚÄÄÄÄÄÄÄÄÄÄ¿                                     ÚÄÄÄÄÄÄÄÄÄÄ¿
   ³          ³                                     ³          ³
   ³  alice   ³<ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>³  david   ³
   ³          ³                                     ³          ³
   ³          ³<ÄÄÄÄ¿                       ÚÄÄÄÄÄÄ>³          ³
   ÀÄÄÄÄÄÄÄÄÄÄÙ     ³                       ³       ÀÄÄÄÄÄÄÄÄÄÄÙ
        ^           ³     ÚÄÄÄÄÄÄÄÄÄÄ¿      ³            ^
        ³           ÀÄÄÄÄ>³          ³<ÄÄÄÄÄÙ            ³
        ³                 ³  origin  ³                   ³
        ³                 ³          ³                   ³
        ³           ÚÄÄÄÄ>³          ³<ÄÄÄÄÄ¿            ³
        v           ³     ÀÄÄÄÄÄÄÄÄÄÄÙ      ³            v
   ÚÄÄÄÄÄÄÄÄÄÄ¿     ³                       ³       ÚÄÄÄÄÄÄÄÄÄÄ¿
   ³          ³<ÄÄÄÄÙ                       ÀÄÄÄÄÄÄ>³          ³
   ³   bob    ³                                     ³  clair   ³
   ³          ³<ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>³          ³
   ³          ³                                     ³          ³
   ÀÄÄÄÄÄÄÄÄÄÄÙ                                     ÀÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

:p.
Each developer pulls and pushes to origin. But besides the centralized push-pull
 relationships, each developer may also pull changes from other peers to form 
sub teams. For example, this might be useful to work together with two or more 
developers on a big new feature, before pushing the work in progress to origin 
prematurely. In the figure above, there are subteams of Alice and Bob, Alice 
and David, and Clair and David.

:p.
Technically, this means nothing more than that Alice has defined a Git remote, 
named bob, pointing to Bob's repository, and vice versa.


.* -------------------------------------
:h2.The main branches
:p.
At the core, the development model is greatly inspired by existing models out 
there. The central repo holds two main branches with an infinite lifetime:

:ul compact.
:li.master
:li.develop
:eul.

:cgraphic.
            (1)             (2)     (3)
  master  ÄÄÄAÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄFÄÄÄÄÄÄÄIÄÄÄÄÄÄÄÄÄÄÄÄ>
              \             /       /
 develop  ÄÄÄÄÄBÄÄÄCÄÄÄDÄÄÄEÄÄÄGÄÄÄHÄÄÄJÄÄÄKÄÄÄÄÄÄ>
                                          (4)


 1) Initial production version
 2) Next production version
 3) Next production version
 4) Work in progress on "next release"
:ecgraphic.

.* :cgraphic.
.*               develop            master
.*                  |                 |
.*                  |                 o Initial production version
.*                  |                /|
.*                  | ,-------------' |
.*                  |/                |
.*                  o                 |
.*                  |                 |
.*                  o                 |
.*                  |                 |
.*                  o                 |
.*                  |                 |
.*                  o                 |
.*                  |\                |
.*                  | `-------------, |
.*                  o                \|
.*                  |                 o Next production version
.*                  o                 |
.*                  |\                |
.*                  | `-------------, |
.*                  o                \|
.*                  |                 o Next production version
.*      Work in     o                 |
.*    progress on   |                 |
.*  "next release"  |                 |
.*                  |                 |
.*                  v                 v
.* :ecgraphic.

:p.
The :hp8.master:ehp8. branch at :hp8.origin:ehp8. should be familiar to every Git user. Parallel to 
the :hp8.master:ehp8. branch, another branch exists called :hp8.develop:ehp8..

:p.
We consider :hp8.origin/master:ehp8. to be the main branch where the source code of HEAD 
always reflects a production-ready state.

:p.
We consider :hp8.origin/develop:ehp8. to be the main branch where the source code of HEAD 
always reflects a state with the latest delivered development changes for the 
next release. Some would call this the "integration branch". This is where any 
automatic nightly builds are built from.

:p.
When the source code in the :hp8.develop:ehp8. branch reaches a stable point and is ready 
to be released, all of the changes should be merged back into :hp8.master:ehp8. somehow 
and then tagged with a release number. How this is done in detail will be 
discussed further on.

:p.
Therefore, each time when changes are merged back into :hp8.master:ehp8., this is a new 
production release by :hp1.definition:ehp1.. We tend to be very strict at this, so that 
theoretically, we could use a Git hook script to automatically build and 
roll-out our software to our production servers everytime there was a commit 
on :hp8.master:ehp8..


.* -------------------------------------
:h2.Supporting branches
:p.
Next to the main branches master and develop, our development model uses a 
variety of supporting branches to aid parallel development between team members, 
ease tracking of features, prepare for production releases and to assist in 
quickly fixing live production problems. Unlike the main branches, these 
branches always have a limited life time, since they will be removed eventually.

:p.
The different types of branches we may use are:

:ul compact.
:li.Feature branches
:li.Release branches
:li.Hotfix branches
:eul.

:p.
Each of these branches have a specific purpose and are bound to strict rules 
as to which branches may be their originating branch and which branches must 
be their merge targets. We will walk through them in a minute.

:p.
By no means are these branches "special" from a technical perspective. The 
branch types are categorized by how we use them. They are of course plain old 
Git branches.

.* -------------------------------------
:h2.Feature branches
.* :artwork align=center name='feature-branch.bmp'.
:cgraphic.
 develop  ÄÄÄAÄÄÄBÄÄÄGÄÄÄHÄÄÄÄÄÄÄIÄÄÄJÄÄÄ>
                  \             /
 feature  ÄÄÄÄÄÄÄÄÄCÄÄÄDÄÄÄEÄÄÄFÄÄÄÄÄÄÄÄÄ>
 branches
:ecgraphic.


.* :cgraphic.
.*               feature
.*               branches          develop
.*                  |                 |
.*                  |                 o
.*                  |                 |
.*                  |                 o
.*                  |                /|
.*                  | ,-------------' |
.*                  |/                o
.*                  o                 |
.*                  |                 |
.*                  o                 o
.*                  |                 |
.*                  o                 |
.*                  |\                |
.*                  | `-------------, |
.*                  |                \|
.*                  |                 o
.*                  |                 |
.*                  |                 |
.*                  v                 v
.* :ecgraphic.

:p.
May branch off from: :hp8.develop:ehp8.
.br
Must merge back into: :hp8.develop:ehp8.
.br
Branch naming convention: anything except :hp8.master:ehp8., :hp8.develop:ehp8., 
:hp8.release-*:ehp8., or :hp8.hotfix-*:ehp8.

:p.
Feature branches (or sometimes called topic branches) are used to develop new 
features for the upcoming or a distant future release. When starting development 
of a feature, the target release in which this feature will be incorporated may 
well be unknown at that point. The essence of a feature branch is that it 
exists as long as the feature is in development, but will eventually be merged 
back into develop (to definitely add the new feature to the upcoming release) 
or discarded (in case of a disappointing experiment).

:p.
Feature branches typically exist in developer repos only, not in :hp8.origin:ehp8..

:h3.Creating a feature branch
:p.
When starting work on a new feature, branch off from the develop branch.

:lm margin=5.
:xmp.
$ git checkout -b myfeature develop
Switched to a new branch "myfeature"
:exmp.
:lm margin=1.

:h3.Incorporating a finished feature on develop
:p.
Finished features may be merged into the develop branch definitely add them to 
the upcoming release:

:lm margin=5.
:xmp.
$ git checkout develop
Switched to branch 'develop'
$ git merge --no-ff myfeature
Updating ea1b82a..05e9557
(Summary of changes)
$ git branch -d myfeature
Deleted branch myfeature (was 05e9557).
$ git push origin develop
:exmp.
:lm margin=1.

:p.
The :hp2.--no-ff:ehp2. flag causes the merge to always create a new commit object, even 
if the merge could be performed with a fast-forward. This avoids losing 
information about the historical existence of a feature branch and groups 
together all commits that together added the feature. Compare:

:cgraphic.

   :hp2.git merge --no--ff:ehp2.

 develop  ÄÄÄAÄÄÄBÄÄÄÄÄÄÄÄÄÄÄFÄÄÄÄÄÄÄ>
                  \         /
 feature  ÄÄÄÄÄÄÄÄÄCÄÄÄDÄÄÄEÄÄÄÄÄÄÄÄÄ>
 branches


   :hp2.git merge:ehp2.
    (plain)

 develop  ÄÄÄAÄÄÄBÄÄÄCÄÄÄDÄÄÄEÄÄÄFÄÄÄ>


   where C, D and E are the feature commits
:ecgraphic.

:p.
In the latter case, it is impossible to see from the Git history which of the 
commit objects together have implemented a feature -- you would have to manually 
read all the log messages. Reverting a whole feature (i.e. a group of commits), 
is a true headache in the latter situation, whereas it is easily done if the 
:hp2.--no-ff:ehp2. flag was used.

:p.
Yes, it will create a few more (empty) commit objects, but the gain is much 
bigger that that cost.

:p.
Unfortunately, I have not found a way to make :hp2.--no-ff:ehp2. the default behaviour of 
:hp2.git merge:ehp2. yet, but it really should be.


.* -------------------------------------
:h2.Release branches
:p.
May branch off from: :hp8.develop:ehp8.
.br
Must merge back into: :hp8.develop:ehp8. and :hp8.master:ehp8.
.br
Branch naming convention: :hp8.release-*:ehp8.

:p.
Release branches support preparation of a new production release. They allow 
for last-minute dotting of i's and crossing t's. Furthermore, they allow for 
minor bug fixes and preparing meta-data for a release (version number, build 
dates, etc.). By doing all of this work on a release branch, the :hp8.develop:ehp8. branch 
is cleared to receive features for the next big release.

:p.
The key moment to branch off a new release branch from :hp8.develop:ehp8. is when develop 
(almost) reflects the desired state of the new release. At least all features 
that are targeted for the release-to-be-built must be merged in to develop at 
this point in time. All features targeted at future releases may not -- they must 
wait until after the release branch is branched off.

:p.
It is exactly at the start of a release branch that the upcoming release gets 
assigned a version number -- not any earlier. Up until that moment, the :hp8.develop:ehp8.
branch reflected changes for the "next release", but it is unclear whether 
that "next release" will eventually become 0.3 or 1.0, until the release branch 
is started. That decision is made on the start of the release branch and is 
carried out by the project's rules on version number bumping.

:h3.Creating a release branch
:p.
Release branches are created from the develop branch. For example, say version 
1.1.5 is the current production release and we have a big release coming up. 
The state of develop is ready for the "next release" and we have decided that 
this will become version 1.2 (rather than 1.1.6 or 2.0). So we branch off and 
give the release branch a name reflecting the new version number:

:lm margin=5.
:xmp.
$ git checkout -b release-1.2 develop
Switched to a new branch "release-1.2"
$ ./bump-version.sh 1.2
Files modified successfully, version bumped to 1.2.
$ git commit -a -m "Bumped version number to 1.2"
[release-1.2 74d9424] Bumped version number to 1.2
1 files changed, 1 insertions(+), 1 deletions(-)
:exmp.
:lm margin=1.

:p.
After creating a new branch and switching to it, we bump the version number. 
Here, :hp2.bump-version.sh:ehp2. is a fictional shell script that changes some files in 
the working copy to reflect the new version. (This can of course be a manual 
change -- the point being that some files change.) Then, the bumped version number 
is committed.

:p.
This new branch may exist there for a while, until the release may be rolled 
out definitely. During that time, bug fixes may be applied in this branch 
(rather than on the :hp8.develop:ehp8. branch). Adding large new features here is strictly 
prohibited. They must be merged into :hp8.develop:ehp8., and therefore, wait for the next 
big release.

:h3.Finishing a release branch
:p.
When the state of the release branch is ready to become a real release, some 
actions need to be carried out. First, the release branch is merged into :hp8.master:ehp8.
(since every commit on :hp8.master:ehp8. is a new release by definition, remember). Next, 
that commit on :hp8.master:ehp8. must be tagged for easy future reference to this 
historical version. Finally, the changes made on the release branch need to be 
merged back into :hp8.develop:ehp8., so that future releases also contain these bug fixes.

:p.
The first two steps in Git:

:lm margin=5.
:xmp.
$ git checkout master
Switched to branch 'master'
$ git merge --no-ff release-1.2
Merge made by recursive.
(Summary of changes)
$ git tag -a 1.2
:exmp.
:lm margin=1.

:p.
The release is now done, and tagged for future reference. You might as well 
use the -s or -u <key> flags to sign your tag cryptographically.

:p.
To keep the changes made in the release branch, we need to merge those back 
into :hp8.develop:ehp8., though. In Git:

:lm margin=5.
:xmp.
$ git checkout develop
Switched to branch 'develop'
$ git merge --no-ff release-1.2
Merge made by recursive.
(Summary of changes)
:exmp.
:lm margin=1.

:p.
This step may well lead to a merge conflict (probably even, since we have 
changed the version number). If so, fix it and commit.

:p.
Now we are really done and the release branch may be removed, since we don't 
need it anymore:

:lm margin=5.
:xmp.
$ git branch -d release-1.2
Deleted branch release-1.2 (was ff452fe).
:exmp.
:lm margin=1.



.* -------------------------------------
:h2.Hotfix branches
:p.
May branch off from: :hp8.master:ehp8.
.br
Must merge back into: :hp8.develop:ehp8. and :hp8.master:ehp8.
.br
Branch naming convention: :hp8.hotfix-*:ehp8.

:p.
Hotfix branches are very much like release branches in that they are also meant 
to prepare for a new production release, albeit unplanned. They arise from the 
necessity to act immediately upon an undesired state of a live production version. 
When a critical bug in a production version must be resolved immediately, a 
hotfix branch may be branched off from the corresponding tag on the master 
branch that marks the production version.

:p.
The essence is that work of team members (on the :hp8.develop:ehp8. branch) can continue, 
while another person is preparing a quick production fix.


:h3.Creating the hotfix branch
:p.
Hotfix branches are created from the :hp8.master:ehp8. branch. For example, say version 
1.2 is the current production release running live and causing troubles due 
to a severe bug. But changes on :hp8.develop:ehp8. are yet unstable. We may then branch 
off a hotfix branch and start fixing the problem:

:lm margin=5.
:xmp.
$ git checkout -b hotfix-1.2.1 master
Switched to a new branch "hotfix-1.2.1"
$ ./bump-version.sh 1.2.1
Files modified successfully, version bumped to 1.2.1.
$ git commit -a -m "Bumped version number to 1.2.1"
[hotfix-1.2.1 41e61bb] Bumped version number to 1.2.1
1 files changed, 1 insertions(+), 1 deletions(-)
:exmp.
:lm margin=1.

:p.
Don't forget to bump the version number after branching off!

:p.
Then, fix the bug and commit the fix in one or more separate commits.

:lm margin=5.
:xmp.
$ git commit -m "Fixed severe production problem"
[hotfix-1.2.1 abbe5d6] Fixed severe production problem
5 files changed, 32 insertions(+), 17 deletions(-)
:exmp.
:lm margin=1.


:h3.Finishing a hotfix branch
:p.
When finished, the bugfix needs to be merged back into :hp8.master:ehp8., but also needs 
to be merged back into :hp8.develop:ehp8., in order to safeguard that the bugfix is 
included in the next release as well. This is completely similar to how 
release branches are finished.

:p.
First, update :hp8.master:ehp8. and tag the release.

:lm margin=5.
:xmp.
$ git checkout master
Switched to branch 'master'
$ git merge --no-ff hotfix-1.2.1
Merge made by recursive.
(Summary of changes)
$ git tag -a 1.2.1
:exmp.
:lm margin=1.

:p.
You might as well use the -s or -u <key> flags to sign your tag cryptographically.

:p.
Next, include the bugfix in :hp8.develop:ehp8., too:

:lm margin=5.
:xmp.
$ git checkout develop
Switched to branch 'develop'
$ git merge --no-ff hotfix-1.2.1
Merge made by recursive.
(Summary of changes)
:exmp.
:lm margin=1.

:p.
The one exception to the rule here is that, :hp2.when a release branch currently 
exists, the hotfix changes need to be merged into that release branch, instead 
of develop:ehp2.. Back-merging the bugfix into the release branch will eventually 
result in the bugfix being merged into :hp8.develop:ehp8. too, when the release branch is 
finished. (If work in :hp8.develop:ehp8. immediately requires this bugfix and cannot wait 
for the release branch to be finished, you may safely merge the bugfix into 
:hp8.develop:ehp8. now already as well.)

:p.
Finally, remove the temporary branch:

:lm margin=5.
:xmp.
$ git branch -d hotfix-1.2.1
Deleted branch hotfix-1.2.1 (was abbe5d6).
:exmp.
:lm margin=1.


:h2.Summary
:p.
While there is nothing really shocking new to this branching model, the "big 
picture" figure that this post began with has turned out to be tremendously 
useful in our projects. It forms an elegant mental model that is easy to 
comprehend and allows team members to develop a shared understanding of the 
branching and releasing processes.

:p.
This document is based on an article found at http://nvie.com/posts/a-successful-git-branching-model/




:euserdoc.

