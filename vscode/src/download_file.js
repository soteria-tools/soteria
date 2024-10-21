const https = require('https');
const fs = require('fs');
const path = require('path');
const AdmZip = require('adm-zip'); // or use `tar` for tarballs

const dlArchiveName = "downloaded_archive.zip"

function ok() {
  return {
    case: "ok",
    ok: undefined
  }
}

function err(str) {
  return {
    case: "error",
    error: str,
  }
}

function downloadFile(url, destinationPath, progress) {
  // Download constiutes 95% of the progress
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(destinationPath);
    https.get(url, (response) => {
      const totalSize = parseInt(response.headers['content-length'] || '0', 10);
      let downloadedSize = 0;
      response.on('data', (chunk) => {
        downloadedSize += chunk.length;
        const percent = (downloadedSize / totalSize) * 100;
        progress.report({
          message: `Downloading binaries... ${Math.round(percent)}%`,
          increment: percent * .95
        });
      })

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
  progress.report({
    message: `Extracting archive...`
  });
  const zip = new AdmZip(archivePath);
  zip.extractAllTo(extractTo, true);
  progress.report({
    message: `Extracting archive... Done`,
    increment: 3
  });
}

async function downloadAndExtract(progress, url, destinationFolder) {
  // Total progress made here is 98%
  fs.mkdirSync(destinationFolder, { recursive: true });
  const archivePath = path.join(destinationFolder, dlArchiveName);
  try {
    await downloadFile(url, archivePath, progress);
    extractArchive(archivePath, destinationFolder);
    await fs.promises.unlink(archivePath);
    return ok()
  } catch (error) {
    console.error('Failed to download or extract file:', error);
    return err("Failed to download or extract files");
  }
}



/**
 * Adds executable permissions to a file (equivalent to `chmod +x`).
 * @param {string} fileName - The name or path of the file to change permissions.
 */
async function makeExec(fileName) {
  try {
    // First, get the current file permissions using fs.stat
    const stats = await fs.promises.stat(fileName);
    // Get the current mode (file permissions)
    const currentMode = stats.mode;

    // Add executable bits (1 << 6 is owner, 1 << 3 is group, 1 << 0 is others)
    const newMode = currentMode | 0o111;
    fs.promises.chmod(fileName, newMode);
    return ok();
  } catch (error) {
    let msg = `Failed to change mode of {fileName}`;
    console.error(msg, error);
    return err(msg)
  }
}

async function rename(src, dest) {
  try {
    await fs.promises.rename(src, dest);
    return ok();
  } catch (error) {
    let msg = `Failed to rename {src} into {dest}`;
    console.error(msg, error);
    return err(msg);
  }
}

globalThis.dlArchiveName = dlArchiveName;
globalThis.fsRename = rename;
globalThis.makeExec = makeExec;
globalThis.downloadAndExtract = downloadAndExtract;