# VHS tape file for recording the chainsaw demo GIF.
# Install VHS: https://github.com/charmbracelet/vhs
# Render: vhs docs/demo.tape
#
# Pixel-perfect rendering. Requires chainsaw on PATH and
# workers-sdk cloned at the path below.

Output docs/demo.gif
Set Shell bash
Set FontFamily "JetBrains Mono"
Set FontSize 16
Set Width 1200
Set Height 700
Set Theme "Monokai"
Set TypingSpeed 45ms
Set Padding 20

# --- Scene 1: Trace wrangler ---

Type "# How much code does wrangler pull in on startup?"
Enter
Sleep 800ms

Type "cd packages/wrangler && chainsaw trace src/index.ts"
Enter
Sleep 3s

# --- Scene 2: Find the cut point ---

Type "# 2.7 MB across 467 modules. Find the cut point:"
Enter
Sleep 800ms

Type "chainsaw trace src/index.ts --cut @cloudflare/workers-utils"
Enter
Sleep 2.5s

# --- Scene 3: Chain analysis ---

Type "# One file gates everything. How does start-dev get pulled in?"
Enter
Sleep 800ms

Type "chainsaw trace src/index.ts --chain src/dev/start-dev.ts"
Enter
Sleep 2.5s

# --- Scene 4: Cut attempt ---

Type "# Two independent paths -- can we cut it?"
Enter
Sleep 800ms

Type "chainsaw trace src/index.ts --cut src/dev/start-dev.ts"
Enter
Sleep 2.5s

# --- Scene 5: Compare with miniflare ---

Type "# No single cut. Compare with a well-structured package:"
Enter
Sleep 800ms

Type "cd ../miniflare && chainsaw trace src/index.ts"
Enter
Sleep 2.5s

Type "# 689 KB, 103 modules, zero third-party deps."
Enter
Sleep 1s

Type "# cargo install chainsaw-cli"
Enter
Sleep 2s
