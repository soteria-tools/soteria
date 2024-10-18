const https = require('https');
const fs = require('fs');
const path = require('path');
const AdmZip = require('adm-zip'); // or use `tar` for tarballs

function downloadFile(url, destinationPath) {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(destinationPath);
    https.get(url, (response) => {
      response.pipe(file);
      file.on('finish', () => {
        file.close(resolve);
      });
    }).on('error', (err) => {
      fs.unlink(destinationPath, () => reject(err));
    });
  });
}

function extractArchive(archivePath, extractTo) {
  const zip = new AdmZip(archivePath);
  zip.extractAllTo(extractTo, true);
}

async function downloadAndExtract(url, destinationFolder) {
  fs.mkdirSync(destinationFolder, { recursive: true });
  const archivePath = path.join(destinationFolder, 'downloaded_archive.zip');
  try {
    await downloadFile(url, archivePath);
    extractArchive(archivePath, destinationFolder);
    await fs.promises.unlink(archivePath);
  } catch (error) {
    console.error('Failed to download or extract file:', error);
  }
}

/**
 * Adds executable permissions to a file (equivalent to `chmod +x`).
 * @param {string} fileName - The name or path of the file to change permissions.
 */
async function makeExec(fileName) {
  // First, get the current file permissions using fs.stat
  const stats = await fs.promises.stat(fileName);
  // Get the current mode (file permissions)
  const currentMode = stats.mode;

  // Add executable bits (1 << 6 is owner, 1 << 3 is group, 1 << 0 is others)
  const newMode = currentMode | 0o111;
  fs.promises.chmod(fileName, newMode);
}

globalThis.fsRename = fs.promises.rename;
globalThis.makeExec = makeExec;
globalThis.downloadAndExtract = downloadAndExtract;