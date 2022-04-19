#220414 Jason S.K.
#this script uses lieer to fetch new email
#cd to config directory
#gmi pushes and pulls new emails
#popd to previous dir
#notmuch new: Find and import new messages to the notmuch database.
pushd $1 ; gmi sync ; popd ; notmuch new
