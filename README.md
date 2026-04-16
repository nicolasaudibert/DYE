# DYE (Design Your Experiment) - PsyToolkit Listening Test Designer

A Shiny application for designing audio perception experiments compatible with [PsyToolkit](https://www.psytoolkit.org/).

If you use the app for your research, please cite:

> Audibert, N. (to appear). DYE (Design Your Experiment) : une interface graphique en ligne pour la conception d’évaluation perceptives avec PsyToolkit. Actes des 36èmes Journées d'Études sur la Parole (JEP 2026), Juin 2026, Montpellier, France.

## ⚠️ Important Notice

This application is **not part of PsyToolkit** and covers only a small subset of PsyToolkit's capabilities. Using this tool **does not exempt you from citing PsyToolkit** in your publications.

**Please also cite PsyToolkit:** See the [citation guidelines](https://www.psytoolkit.org/faq.html#citations).

## Features

### Audio Stimulus Management
- Import stimulus lists from Excel, CSV, or TXT files
- Upload MP3 files directly
- Support for predefined presentation orders (via ZIP archive)
- Metadata columns for experimental conditions

### Response Types
- **Forced Choice**: 2-8 button options with customizable labels, images, and colors
- **Likert Scales**: 1-5 scales with 3-10 points each, customizable endpoints

### Instructions Editor
- Rich text editor with formatting options
- Direct image upload option
- Optional instructions display

### Live Preview
- Real-time visualization of experiment layout
- Alignment grid for precise positioning
- Instructions and response page preview

### Export
- Generates ready-to-use PsyToolkit ZIP file
- Includes all images and configuration
- Optional MP3 file inclusion
- Step-by-step import instructions

## Installation

### Prerequisites

- R (version 4.0 or higher recommended)
- RStudio (optional but recommended)

### Required R Packages

Install the required packages by running:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "colourpicker",
  "DT",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "stringr",
  "tibble",
  "ggplot2",
  "grid",
  "gridExtra",
  "png",
  "zip",
  "markdown"
))
```

### Optional Packages

For instructions page rendering (rich text to PNG conversion):

```r
install.packages("webshot2")
webshot2::install_chromium()
```

## Running the Application

### Option 1: From RStudio

1. Open `app.R` in RStudio
2. Click the "Run App" button

### Option 2: From R Console

```r
# Set working directory to the application folder
setwd("/path/to/psytoolkit_designer")

# Run the application
shiny::runApp()
```

### Option 3: Specify the Path Directly

```r
shiny::runApp("/path/to/psytoolkit_designer")
```

## Project Structure

```
psytoolkit_designer/
├── app.R                          # Main application file
├── shiny_app_modules/
│   ├── mod_audio_files.R          # Audio files management
│   ├── mod_forced_choice.R        # Forced choice configuration
│   ├── mod_likert_scales.R        # Likert scales configuration
│   ├── mod_instructions.R         # Instructions editor
│   ├── mod_settings.R             # Global settings
│   ├── mod_preview.R              # Live preview
│   ├── mod_export.R               # Export functionality
│   └── mod_project.R              # Project management
└── README.md                      # This file
```

## Usage Workflow

1. **Audio Files Tab**: Import your stimulus list and/or upload MP3 files
2. **Forced Choice Tab**: Configure response buttons (enable and customize)
3. **Likert Scales Tab**: Add rating scales if needed
4. **Instructions Tab**: Create participant instructions
5. **Settings Tab**: Adjust display resolution, colors, timeouts
6. **Live Preview Tab**: Verify your experiment layout
7. **Export Tab**: Generate files and download ZIP

## Importing to PsyToolkit

1. Go to PsyToolkit → Create → Create new experiment
2. Choose "Method 2: From a PsyToolkit experiment file (zip format)"
3. Upload the generated ZIP file
4. Click "Create experiment from an uploaded PsyToolkit experiment file"
5. Click "Compile" to build the experiment

## Predefined Presentation Orders

For experiments requiring controlled stimulus presentation:

1. Create multiple stimulus list files (one per order)
2. Package them in a ZIP archive
3. Import the ZIP in the Audio Files tab
4. Each participant will be randomly assigned one order

You might consider using [MILP_Randomizer](https://github.com/nicolasaudibert/MILP_Randomizer) to generate pseudo-random orders from a list of stimuli and a set of constraints.

## Running from a PsyToolkit Survey

To embed the experiment in a survey, add to your survey code:

```
l: expe
t: experiment
- psytoolkit_experiment_YourProjectName
```

## Limitations

- Likert scales appear sequentially (PsyToolkit `rate` command limitation)
- Maximum 8 forced choice options
- Maximum 5 Likert scales per trial
- Audio files must be in MP3 format

## Troubleshooting

### Instructions image not generating
- Install `webshot2` package: `install.packages("webshot2")`
- Install Chromium: `webshot2::install_chromium()`

### Package loading errors
- Ensure all required packages are installed
- Update R and packages to latest versions

### ZIP file issues
- Check that all referenced MP3 files exist
- Verify stimulus list format matches expected columns

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This project is provided as-is for academic and research purposes.

## Acknowledgments

This application generates experiments for [PsyToolkit](https://www.psytoolkit.org/), created by Pr. Gijsbert Stoet. If you use experiments created with this tool, please cite PsyToolkit according to their [citation guidelines](https://www.psytoolkit.org/faq.html#citations).
