#!/usr/bin/env Rscript
# ==============================================================================
# SCRIPT: encrypt_docs.R
# ZWECK:  Client-seitige Verschlüsselung (AES-256-CBC + PBKDF2-HMAC-SHA256)
#         aller gerenderten Quarto-HTML-Seiten in docs/ sowie Bereinigung von
#         docs/search.json zum Schutz sensibler Projektdaten (Digit Soil / OptiFert).
#         Wird automatisch als Quarto `post-render` Hook ausgeführt.
# ==============================================================================

suppressPackageStartupMessages({
  library(openssl)
  library(jsonlite)
})

# 1. Passwort ermitteln (1. Umgebungsvariable, 2. .site_password Datei)
get_site_password <- function() {
  env_pw <- Sys.getenv("OPTIFERT_SITE_PASSWORD", unset = "")
  if (nzchar(trimws(env_pw))) {
    return(trimws(env_pw))
  }
  pw_file <- ".site_password"
  if (file.exists(pw_file)) {
    pw <- trimws(paste(readLines(pw_file, warn = FALSE), collapse = ""))
    if (nzchar(pw)) return(pw)
  }
  # Fallback-Initialisierung falls noch keine .site_password existiert
  default_pw <- "OptiFert2026"
  writeLines(default_pw, pw_file)
  message("[encrypt_docs] Hinweis: '.site_password' wurde mit Standard-Passwort initialisiert.")
  return(default_pw)
}

# 2. Standardkonformes PBKDF2-HMAC-SHA256 (1 Block = 32 Bytes = 256 Bit) in reinem R
pbkdf2_sha256 <- function(password_raw, salt_raw, iterations = 10000L) {
  u <- sha256(c(salt_raw, as.raw(c(0x00, 0x00, 0x00, 0x01))), key = password_raw)
  out <- u
  if (iterations > 1L) {
    for (i in 2:iterations) {
      u <- sha256(u, key = password_raw)
      out <- xor(out, u)
    }
  }
  as.raw(out)
}

# 3. HTML-Template mit WebCrypto-Entschlüsselung und Session-Caching
build_encrypted_html <- function(title, salt_b64, iv_b64, ct_b64, iterations = 10000L) {
  sprintf('<!DOCTYPE html>
<!-- OPTIFERT-ENCRYPTED -->
<html lang="de">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="robots" content="noindex, nofollow">
  <title>%s – Geschützter Bereich</title>
  <style>
    :root {
      --bg: #f4f6f8;
      --card: #ffffff;
      --primary: #1f4e5f;
      --primary-hover: #163845;
      --border: #dce1e7;
      --text: #2c3e50;
      --error: #c0392b;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      background: var(--bg);
      font-family: system-ui, -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      color: var(--text);
      padding: 1.5rem;
    }
    .auth-card {
      background: var(--card);
      max-width: 420px;
      width: 100%%;
      padding: 2.2rem 2rem;
      border-radius: 10px;
      box-shadow: 0 10px 28px rgba(0, 0, 0, 0.08);
      border: 1px solid var(--border);
      text-align: center;
    }
    .auth-badge {
      display: inline-block;
      background: #e8f1f5;
      color: var(--primary);
      font-size: 0.78rem;
      font-weight: 600;
      letter-spacing: 0.04em;
      text-transform: uppercase;
      padding: 0.3rem 0.75rem;
      border-radius: 999px;
      margin-bottom: 1rem;
    }
    h1 {
      font-size: 1.35rem;
      margin: 0 0 0.5rem 0;
      color: var(--primary);
    }
    p {
      font-size: 0.92rem;
      line-height: 1.5;
      color: #5a6a7e;
      margin: 0 0 1.5rem 0;
    }
    form {
      display: flex;
      flex-direction: column;
      gap: 0.85rem;
    }
    input[type="password"] {
      width: 100%%;
      padding: 0.75rem 0.9rem;
      font-size: 1rem;
      border: 1px solid var(--border);
      border-radius: 6px;
      outline: none;
      transition: border-color 0.15s ease;
    }
    input[type="password"]:focus {
      border-color: var(--primary);
      box-shadow: 0 0 0 3px rgba(31, 78, 95, 0.12);
    }
    button {
      padding: 0.78rem 1rem;
      font-size: 0.98rem;
      font-weight: 600;
      color: #ffffff;
      background: var(--primary);
      border: none;
      border-radius: 6px;
      cursor: pointer;
      transition: background 0.15s ease;
    }
    button:hover {
      background: var(--primary-hover);
    }
    .error-msg {
      color: var(--error);
      font-size: 0.86rem;
      min-height: 1.2rem;
      margin-top: 0.4rem;
    }
  </style>
</head>
<body>
  <div class="auth-card" id="auth-box" style="visibility:hidden;">
    <div class="auth-badge">Agroscope / ZHAW – OptiFert</div>
    <h1>Geschützter Forschungsbericht</h1>
    <p>Dieser Bericht enthält vertrauliche Projektdaten. Bitte geben Sie das Projektpasswort ein, um die Seite zu entschlüsseln.</p>
    <form id="auth-form">
      <input type="password" id="pw-input" placeholder="Passwort eingeben..." autocomplete="current-password" required autofocus>
      <button type="submit" id="submit-btn">Bericht entschlüsseln</button>
    </form>
    <div class="error-msg" id="error-msg"></div>
  </div>

  <script>
  (function() {
    const PAYLOAD = {
      salt: "%s",
      iv: "%s",
      ct: "%s",
      iter: %d
    };
    const STORAGE_KEY = "optifert_site_pw_v1";

    function b64ToBytes(b64) {
      const bin = atob(b64);
      const len = bin.length;
      const bytes = new Uint8Array(len);
      for (let i = 0; i < len; i++) bytes[i] = bin.charCodeAt(i);
      return bytes;
    }

    async function decryptWithPassword(pw) {
      const enc = new TextEncoder();
      const salt = b64ToBytes(PAYLOAD.salt);
      const iv   = b64ToBytes(PAYLOAD.iv);
      const ct   = b64ToBytes(PAYLOAD.ct);

      const baseKey = await window.crypto.subtle.importKey(
        "raw", enc.encode(pw), "PBKDF2", false, ["deriveKey"]
      );
      const aesKey = await window.crypto.subtle.deriveKey(
        { name: "PBKDF2", salt: salt, iterations: PAYLOAD.iter, hash: "SHA-256" },
        baseKey,
        { name: "AES-CBC", length: 256 },
        false,
        ["decrypt"]
      );
      const plainBuf = await window.crypto.subtle.decrypt(
        { name: "AES-CBC", iv: iv },
        aesKey,
        ct
      );
      const dec = new TextDecoder("utf-8");
      const html = dec.decode(plainBuf);
      if (!html.includes("<html") && !html.includes("<!DOCTYPE")) {
        throw new Error("Invalid plaintext header");
      }
      return html;
    }

    function renderDecrypted(html) {
      document.open();
      document.write(html);
      document.close();
    }

    async function init() {
      const cachedPw = sessionStorage.getItem(STORAGE_KEY);
      if (cachedPw) {
        try {
          const html = await decryptWithPassword(cachedPw);
          renderDecrypted(html);
          return;
        } catch (e) {
          sessionStorage.removeItem(STORAGE_KEY);
        }
      }
      const box = document.getElementById("auth-box");
      if (box) box.style.visibility = "visible";

      const form = document.getElementById("auth-form");
      const input = document.getElementById("pw-input");
      const err = document.getElementById("error-msg");
      const btn = document.getElementById("submit-btn");

      form.addEventListener("submit", async function(ev) {
        ev.preventDefault();
        err.textContent = "";
        btn.disabled = true;
        btn.textContent = "Entschlüssele...";
        const pw = input.value;
        try {
          const html = await decryptWithPassword(pw);
          sessionStorage.setItem(STORAGE_KEY, pw);
          renderDecrypted(html);
        } catch (e) {
          err.textContent = "Ungültiges Passwort. Bitte erneut versuchen.";
          btn.disabled = false;
          btn.textContent = "Bericht entschlüsseln";
          input.select();
        }
      });
    }

    init();
  })();
  </script>
</body>
</html>', title, salt_b64, iv_b64, ct_b64, iterations)
}

# 4. Hauptfunktion: Alle HTML-Dateien in output_dir verschlüsseln
out_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", unset = "docs")
if (!dir.exists(out_dir)) {
  message("[encrypt_docs] Ausgabeverzeichnis '", out_dir, "' existiert nicht – überspringe.")
  quit(save = "no", status = 0)
}

pw <- get_site_password()
pw_raw <- charToRaw(enc2utf8(pw))
iterations <- 10000L

html_files <- list.files(out_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
# Ignoriere *_files/ Bibliotheks-Assets (z.B. Pandoc/Plotly Widget-Unterdateien falls vorhanden)
html_files <- html_files[!grepl("_files/|site_libs/", html_files)]

encrypted_count <- 0L
for (f in html_files) {
  raw_lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  full_html <- paste(raw_lines, collapse = "\n")
  
  # Bereits verschlüsselte Dateien überspringen
  if (grepl("<!-- OPTIFERT-ENCRYPTED -->", full_html, fixed = TRUE)) {
    next
  }
  
  salt <- rand_bytes(16)
  iv   <- rand_bytes(16)
  key  <- pbkdf2_sha256(pw_raw, salt, iterations = iterations)
  
  plain_raw <- charToRaw(enc2utf8(full_html))
  ct_raw    <- aes_cbc_encrypt(plain_raw, key = key, iv = iv)
  
  wrapper <- build_encrypted_html(
    title      = "OptiFert",
    salt_b64   = base64_encode(salt),
    iv_b64     = base64_encode(iv),
    ct_b64     = base64_encode(ct_raw),
    iterations = iterations
  )
  
  writeLines(wrapper, f, useBytes = TRUE)
  encrypted_count <- encrypted_count + 1L
}

# 5. Suchindex (search.json) bereinigen, damit keine Klartext-Auszüge geleakt werden
search_json_path <- file.path(out_dir, "search.json")
if (file.exists(search_json_path)) {
  writeLines("[]", search_json_path, useBytes = TRUE)
  message("[encrypt_docs] Klartext-Suchindex '", search_json_path, "' bereinigt.")
}

message(sprintf("[encrypt_docs] %d HTML-Dateien in '%s/' erfolgreich mit AES-256-CBC verschlüsselt.", encrypted_count, out_dir))
