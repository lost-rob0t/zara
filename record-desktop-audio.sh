#!/usr/bin/env bash

# Desktop Audio Recorder
# Records desktop audio and saves as WAV or MP3 format

set -e

# Default values
DEFAULT_DURATION=30
DEFAULT_FORMAT="wav"
DEFAULT_OUTPUT="desktop-recording"
DEFAULT_DEVICE="default"
MAX_FILE_SIZE=5242880  # 5MiB in bytes

# Help message
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Records desktop audio and saves as WAV or MP3 format.

OPTIONS:
    -d, --duration SECONDS    Recording duration in seconds (default: $DEFAULT_DURATION)
    -f, --format FORMAT       Output format: wav or mp3 (default: $DEFAULT_FORMAT)
    -o, --output FILENAME     Output filename without extension (default: $DEFAULT_OUTPUT)
    -s, --max-size SIZE       Maximum file size in MiB (default: 5)
    -l, --list-devices        List available audio devices
    -h, --help               Show this help message

EXAMPLES:
    $(basename "$0")                           # Record 30 seconds as WAV (max 5MiB)
    $(basename "$0") -d 60 -f mp3              # Record 60 seconds as MP3 (max 5MiB)
    $(basename "$0") -s 10 -o my-recording     # Record with 10MiB max size
    $(basename "$0") -o my-recording -f mp3    # Record as MP3 with custom name (max 5MiB)
    $(basename "$0") -l                        # List available devices
EOF
}

# List available audio devices
list_devices() {
    echo "Available audio devices:"
    if command -v pactl >/dev/null 2>&1; then
        pactl list sources | grep -E "Name:|Description:" | paste - -
    elif command -v arecord -l >/dev/null 2>&1; then
        arecord -l
    else
        echo "No audio device listing utility found"
        return 1
    fi
}

# Check dependencies
check_deps() {
    local missing=()
    
    if ! command -v ffmpeg >/dev/null 2>&1; then
        missing+=("ffmpeg")
    fi
    
    if [[ "$FORMAT" == "mp3" ]] && ! command -v lame >/dev/null 2>&1; then
        missing+=("lame")
    fi
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        echo "Missing dependencies: ${missing[*]}"
        echo "Install with: sudo apt install ${missing[*]}"
        return 1
    fi
}

# Parse command line arguments
DURATION=$DEFAULT_DURATION
FORMAT=$DEFAULT_FORMAT
OUTPUT=$DEFAULT_OUTPUT
MAX_SIZE_MIB=5

while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--duration)
            DURATION="$2"
            shift 2
            ;;
        -f|--format)
            FORMAT="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT="$2"
            shift 2
            ;;
        -s|--max-size)
            MAX_SIZE_MIB="$2"
            shift 2
            ;;
        -l|--list-devices)
            list_devices
            exit 0
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Convert max size from MiB to bytes
MAX_SIZE_BYTES=$((MAX_SIZE_MIB * 1024 * 1024))

# Validate format
if [[ "$FORMAT" != "wav" && "$FORMAT" != "mp3" ]]; then
    echo "Error: Format must be 'wav' or 'mp3'"
    usage
    exit 1
fi

# Validate duration
if ! [[ "$DURATION" =~ ^[0-9]+$ ]]; then
    echo "Error: Duration must be a positive integer"
    usage
    exit 1
fi

# Validate max size
if ! [[ "$MAX_SIZE_MIB" =~ ^[0-9]+$ ]] || [[ "$MAX_SIZE_MIB" -lt 1 ]]; then
    echo "Error: Max size must be a positive integer (MiB)"
    usage
    exit 1
fi

# Check dependencies
check_deps

# Determine audio source based on system
if command -v pactl >/dev/null 2>&1; then
    # PulseAudio system
    AUDIO_SOURCE=$(pactl get-default-source)
    if [[ -z "$AUDIO_SOURCE" ]]; then
        AUDIO_SOURCE="default"
    fi
    INPUT_ARGS="-f pulse -i $AUDIO_SOURCE"
elif command -v arecord >/dev/null 2>&1; then
    # ALSA system
    INPUT_ARGS="-f alsa -i $DEFAULT_DEVICE"
else
    echo "Error: No supported audio system found (PulseAudio or ALSA)"
    exit 1
fi

# Record audio
OUTPUT_FILE="${OUTPUT}.${FORMAT}"
FULLPATH_OUTPUT="$(pwd)/${OUTPUT_FILE}"

echo "Recording desktop audio (max ${MAX_SIZE_MIB}MiB, ${DURATION}s max)..."
echo "Format: $FORMAT"
echo "Output: $FULLPATH_OUTPUT"
echo "Audio source: $AUDIO_SOURCE"

# Function to check file size and stop recording if needed
monitor_file_size() {
    local file="$1"
    local max_size="$2"
    
    while [[ -f "$file" ]]; do
        if [[ -s "$file" ]]; then
            local current_size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null || echo 0)
            if [[ $current_size -gt $max_size ]]; then
                echo "File size limit reached ($(($current_size / 1024 / 1024))MiB > $(($max_size / 1024 / 1024))MiB)"
                pkill -f "ffmpeg.*$file"
                break
            fi
        fi
        sleep 0.5
    done
}

if [[ "$FORMAT" == "wav" ]]; then
    # Start recording in background
    ffmpeg -y $INPUT_ARGS -t "$DURATION" -acodec pcm_s16le "$OUTPUT_FILE" &
    FFMPEG_PID=$!
    
    # Monitor file size
    monitor_file_size "$OUTPUT_FILE" "$MAX_SIZE_BYTES" &
    MONITOR_PID=$!
    
    # Wait for ffmpeg to finish or be killed
    wait $FFMPEG_PID 2>/dev/null || true
    kill $MONITOR_PID 2>/dev/null || true
    
else
    # First record as WAV, then convert to MP3
    TEMP_WAV="${OUTPUT}.temp.wav"
    
    # Start recording in background
    ffmpeg -y $INPUT_ARGS -t "$DURATION" -acodec pcm_s16le "$TEMP_WAV" &
    FFMPEG_PID=$!
    
    # Monitor file size
    monitor_file_size "$TEMP_WAV" "$MAX_SIZE_BYTES" &
    MONITOR_PID=$!
    
    # Wait for ffmpeg to finish or be killed
    wait $FFMPEG_PID 2>/dev/null || true
    kill $MONITOR_PID 2>/dev/null || true
    
    # Convert to MP3 if temp file exists and has content
    if [[ -s "$TEMP_WAV" ]]; then
        lame "$TEMP_WAV" "$OUTPUT_FILE"
        rm "$TEMP_WAV"
    else
        echo "No audio data recorded"
        rm -f "$TEMP_WAV"
        exit 1
    fi
fi

# Check if output file exists and has content
if [[ -f "$OUTPUT_FILE" && -s "$OUTPUT_FILE" ]]; then
    echo "Recording saved to: $FULLPATH_OUTPUT"
    
    # Show file info
    if command -v file >/dev/null 2>&1; then
        echo "File info: $(file "$OUTPUT_FILE")"
    fi
    
    if command -v ls >/dev/null 2>&1; then
        local actual_size=$(ls -lh "$OUTPUT_FILE" | awk '{print $5}')
        echo "File size: $actual_size"
        
        # Show if size limit was reached
        local size_bytes=$(stat -f%z "$OUTPUT_FILE" 2>/dev/null || stat -c%s "$OUTPUT_FILE" 2>/dev/null || echo 0)
        if [[ $size_bytes -gt $((MAX_SIZE_BYTES - 1024)) ]]; then
            echo "Size limit reached"
        fi
    fi
else
    echo "No audio data recorded or file was empty"
    exit 1
fi