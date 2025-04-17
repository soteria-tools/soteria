###
### This script is for when I want to auto-fetch the latest version from the GitHub API.
###

set -ex

# Variables
REPO="soteria-tools/soteria"
source secrets.env
ARTIFACTS=("macos-latest-package" "macos-13-package")

# Get the latest workflow run
LATEST_RUN_ID=$(curl -s -H "Authorization: token $TOKEN" \
  https://api.github.com/repos/$REPO/actions/runs?per_page=1 \
  | jq -r '.workflow_runs[0].id')
  
echo "LATEST_RUN_ID: $LATEST_RUN_ID"

for ARTIFACT_NAME in "${ARTIFACTS[@]}"; do
  echo "Processing artifact: $ARTIFACT_NAME"

  # Get the artifact ID for the artifact with the specific name
  ARTIFACT_ID=$(curl -s -H "Authorization: token $TOKEN" \
    https://api.github.com/repos/$REPO/actions/runs/$LATEST_RUN_ID/artifacts \
    | jq -r ".artifacts[] | select(.name==\"$ARTIFACT_NAME\") | .id")

  echo "ARTIFACT_ID: $ARTIFACT_ID"

  # Download the artifact
  curl -s -H "Authorization: token $TOKEN" \
    -L https://api.github.com/repos/$REPO/actions/artifacts/$ARTIFACT_ID/zip \
    -o $ARTIFACT_NAME.zip

  scp $ARTIFACT_NAME.zip $SSH_USER@$SSH_HOST:$SSH_PATH
  
  rm $ARTIFACT_NAME.zip
  
  echo "Pushed $ARTIFACT_NAME.zip"
  
done