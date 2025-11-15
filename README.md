# OKLCH Color Converter

A web-based color converter and visualization tool for the OKLCH color space, built with Elm. This application demonstrates color space conversion, gamut mapping, and includes an interactive CIE 1931 xy chromaticity diagram.

## Features

- **OKLCH Color Space Controls**: Interactive sliders for Lightness (L), Chroma (C), and Hue (H)
- **Dual Color Space Support**:
  - sRGB conversion and gamut mapping
  - Display P3 conversion and gamut mapping
- **Visual Color Display**:
  - CSS `oklch()` native browser rendering
  - Computed sRGB and P3 color previews
  - Hex color codes for sRGB
- **CIE 1931 xy Chromaticity Diagram**:
  - Spectral locus visualization
  - sRGB and Display P3 gamut triangles
  - Real-time plotting of current color coordinates
- **Gamut Mapping**: Binary search algorithm to fit out-of-gamut colors into target color spaces
- **URL Sharing**: Share specific colors via URL query parameters (e.g., `?l=0.46&c=0.3&h=200`)
- **Mobile Optimized**: Responsive design with touch-friendly controls

## Getting Started

### Prerequisites

- [pnpm](https://pnpm.io/) package manager
- [Elm](https://elm-lang.org/) 0.19.1

### Development

```bash
# Install dependencies
pnpm install

# Start development server (opens browser at http://localhost:3000)
pnpm run start
```

### Build for Production

```bash
# Compile optimized Elm code
pnpm run build
```

### Deploy to GitHub Pages

Create a separate `gh-pages` branch:

```bash
git checkout --orphan gh-pages
git reset .
echo "oklch.hoshinotsuyoshi.com" > CNAME
git add -f elm.js index.html CNAME
git commit -m "Deploy to GitHub Pages"
git push --force-with-lease origin gh-pages
```

## Project Structure

```
.
├── src/
│   ├── Main.elm                  # Main application entry point
│   ├── Oklch.elm                 # Color conversion and gamut mapping
│   └── ChromaticityDiagram.elm   # CIE 1931 xy diagram rendering
├── index.html                     # HTML template with reset CSS
├── elm.json                       # Elm dependencies
├── package.json                   # Node.js dependencies
└── LICENSE                        # MIT License
```

## Color Conversion Pipeline

1. **OKLCH** (user input) → Oklab (cylindrical to rectangular)
2. **Oklab** → XYZ (D65 illuminant)
3. **XYZ** → Linear RGB (sRGB or P3 matrix)
4. **Linear RGB** → Gamma-corrected RGB (sRGB or P3 transfer function)
5. **Gamut Mapping** (if needed): Binary search to reduce chroma until color fits in target gamut

## Browser Compatibility

- Modern browsers with CSS `oklch()` support (Chrome 111+, Safari 15.4+)
- Display P3 color space support (Safari, Chrome on wide-gamut displays)
- Mobile browsers (iOS Safari, Chrome Mobile)

## License

MIT License - see [LICENSE](LICENSE) file for details

## Acknowledgments

- Color conversion math based on [Oklab color space](https://bottosson.github.io/posts/oklab/) by Björn Ottosson
- CIE 1931 color matching functions from standard observer data
