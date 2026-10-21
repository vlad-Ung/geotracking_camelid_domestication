from qgis.core import *
import os
import re
from glob import glob

species = "all_camels"

# Empty list to store the shapefile layers.
layers = []

# Determine directory path.
vdir = os.path.join(
    QgsProject.instance().homePath(), f"inputs/camel_data/{species}_splits"
)

# Walk the vector directory and store each path in the layers list.
for x in os.listdir(vdir):
    if x.endswith(".shp"):
        y = os.path.join(vdir, x)
        layers.append(y)
# Sort ascending by numeric indexing (now it matches the sorting of the bandwidths).
layers = sorted(layers, key=lambda x: int(re.search(r"\d+", x).group()))

# Make the layer paths into qgis layers.
for i in range(0, len(layers)):  # range is exclusive of the last element.
    layers[i] = QgsVectorLayer(layers[i], f"{species}_{i+1}", "ogr")

# Store the bandwidths from the bandwidth layer in list.
# Select layer with bandwidths by name.
bandwidth_layer = QgsProject.instance().mapLayersByName(f"{species}_bandwidth")[0]

# Initialise empty bandwidths list to access later.
bandwidths = []

# Iterate over each feature in the bandwidth_layer and append the
# feature from the 5th field (0 indexing) to the bandwidths list.
for feature in bandwidth_layer.getFeatures():
    bandwidths.append(feature.attributes()[4])

# Iterate over the shapefiles list and run the Heatmap tool for each iteration.
for i in range(0, len(layers)):
    # if it's the first feature do a buffer.
    if i == 0:
        buffer = processing.run(
            "native:buffer",
            {
                "INPUT": layers[i],
                "DISTANCE": 50000,
                "SEGMENTS": 5,
                "END_CAP_STYLE": 0,
                "JOIN_STYLE": 0,
                "MITER_LIMIT": 2,
                "DISSOLVE": True,
                "SEPARATE_DISJOINT": False,
                "OUTPUT": "TEMPORARY_OUTPUT",
            },
        )["OUTPUT"]

        # Load the tmp polygon in canvas.
        buffer.setName(f"d{i+1}")  # change its name here to know which one.
        QgsProject.instance().addMapLayer(buffer)
    else:
        # First compile the heatmap.
        heatmap = processing.run(
            "qgis:heatmapkerneldensityestimation",
            {
                "INPUT": layers[i],
                "RADIUS": bandwidths[i],
                "RADIUS_FIELD": "",
                "PIXEL_SIZE": 1000,
                "WEIGHT_FIELD": "",
                "KERNEL": 0,
                "DECAY": 0,
                "OUTPUT_VALUE": 0,
                "OUTPUT": "TEMPORARY_OUTPUT",
            },
        )["OUTPUT"]

        raster = QgsRasterLayer(heatmap, "heatmap")

        # Get raster path to feed in next tool.
        provider = raster.dataProvider()
        path = provider.dataSourceUri()
        
        # Then polygonise the heatmap.
        polygon = processing.run(
            "gdal:polygonize",
            {
                "INPUT": path,
                "BAND": 1,
                "FIELD": "DN",
                "EIGHT_CONNECTEDNESS": False,
                "EXTRA": "",
                "OUTPUT": "TEMPORARY_OUTPUT",
            },
        )["OUTPUT"]

        vector = QgsVectorLayer(polygon, "polygon", "ogr")

        path = vector.dataProvider().dataSourceUri()

        # Dissolve the trimmed polygon.
        dissolved = processing.run(
            "native:dissolve",
            {
                "INPUT": path,
                "FIELD": [],
                "SEPARATE_DISJOINT": False,
                "OUTPUT": "TEMPORARY_OUTPUT",
            },
        )["OUTPUT"]

        # Load the tmp polygon in canvas.
        dissolved.setName(f"d{i+1}")  # change its name here to know which one.
        QgsProject.instance().addMapLayer(dissolved)
