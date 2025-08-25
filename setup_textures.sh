#!/bin/bash

GRASS="resource_pack/assets/minecraft/textures/block/grass_block_top.png"
WATER="resource_pack/assets/minecraft/textures/block/water_still.png"

GRASS_OUT="resource_pack/assets/minecraft/textures/block/grass_block_top_green.png"
WATER_OUT="resource_pack/assets/minecraft/textures/block/water_still_blue.png"

convert "$GRASS" -fill "green" -colorize 50% "$GRASS_OUT"

convert "$WATER" -fill "blue" -colorize 50% "$WATER_OUT"

echo "Tinted textures saved as:"
echo "  $GRASS_OUT"
echo "  $WATER_OUT"