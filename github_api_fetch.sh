###
### This script is for when I want to auto-fetch the latest version from the GitHub API.
###

# Variables
REPO="org/repo"
ARTIFACT_NAME="name"
TOKEN="YOUR_GITHUB_TOKEN"

# Get the latest workflow run
LATEST_RUN_ID=$(curl -s -H "Authorization: token $TOKEN" \
  https://api.github.com/repos/$REPO/actions/runs?per_page=1 \
  | jq -r '.workflow_runs[0].id')

# Get the artifact ID for the artifact with the specific name
ARTIFACT_ID=$(curl -s -H "Authorization: token $TOKEN" \
  https://api.github.com/repos/$REPO/actions/runs/$LATEST_RUN_ID/artifacts \
  | jq -r ".artifacts[] | select(.name==\"$ARTIFACT_NAME\") | .id")

# Download the artifact
curl -s -H "Authorization: token $TOKEN" \
  -L https://api.github.com/repos/$REPO/actions/artifacts/$ARTIFACT_ID/zip \
  -o artifact.zip

# Compute the SHA256 hash of the artifact
SHA256=$(shasum -a 256 artifact.zip | awk '{print $1}')

# Output the SHA256
echo "SHA256: $SHA256"