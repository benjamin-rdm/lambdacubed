#!/bin/bash

WATER="resource_pack/assets/minecraft/textures/block/water_still.png"

WATER_OUT="resource_pack/assets/minecraft/textures/block/water_still_blue.png"

convert "$WATER" -fill "blue" -colorize 50% "$WATER_OUT"

echo "Tinted textures saved as:"
echo "  $WATER_OUT"
