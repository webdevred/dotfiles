import subprocess
import json

default_sink = subprocess.Popen(["pactl get-default-sink"], shell=True, stdout=subprocess.PIPE).stdout.read()

sinks = subprocess.Popen(["pactl -f json list sinks"], shell=True, stdout=subprocess.PIPE).stdout.read()

def coolFormat(volumePositions) :
    positionParts = volumePositions.split("-")
    parts = []
    for position in positionParts :
        parts.append(position[0].upper())
    return "".join(parts)

def all_equal(items):
    return len( set( items ) ) == 1

for sink in json.loads(sinks) :
    if sink["name"] == default_sink.decode('ascii').strip() :
        result = [sink["description"][:30]]
        if sink["mute"] == True :
            result.append("Vol: mute")
        else :
            volumes = {}
            for volume_pos,volume_dict in sink["volume"].items() :
                volumes[coolFormat(volume_pos)] = volume_dict["value_percent"]
            if all_equal(volumes.values()) :
                result.append("Vol: " + list(volumes.values())[0])
            else :
                for volumePos, volume in volumes.items() :
                    result.append(volumePos + ": "+ volume)
        break

print(" | ".join(result))
