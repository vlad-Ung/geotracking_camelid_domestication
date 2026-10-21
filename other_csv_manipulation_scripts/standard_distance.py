from shapely.geometry import Point
import math


def standard_distance(shapefile):

    ## get the geometry type from the first record
    geom_type = shapefile.geom_type[0]

    ## required for the formula
    num_features = len(shapefile)

    ## for Point geometry
    if geom_type == "Point":
        ## get all x and y values
        shapefile["x"] = shapefile.geometry.x
        shapefile["y"] = shapefile.geometry.y

    ## for LineString geometry
    elif geom_type == "LineString":
        ## get all x and y values for the midpoints
        shapefile["midpoint"] = shapefile.geometry.interpolate(0.5, normalized=True)
        shapefile["x"] = shapefile["midpoint"].x
        shapefile["y"] = shapefile["midpoint"].y

    ## for Polygon geometry
    elif geom_type == "Polygon":
        ## get all x and y values for the centroids
        shapefile["centroid"] = shapefile.geometry.centroid
        shapefile["x"] = shapefile["centroid"].x
        shapefile["y"] = shapefile["centroid"].y

    ## get the mean of the x and y values
    mean_x = shapefile["x"].mean()
    mean_y = shapefile["y"].mean()

    ## calculate the sum of the squared differences for x and y
    sum_of_sq_diff_x = sum([math.pow(x - mean_x, 2) for x in shapefile["x"]])
    sum_of_sq_diff_y = sum([math.pow(y - mean_y, 2) for y in shapefile["y"]])

    ## get the sum of the results
    sum_of_results = (sum_of_sq_diff_x / num_features) + (
        sum_of_sq_diff_y / num_features
    )

    ## get the square root of the sum of the results to find the standard distance
    standard_distance = math.sqrt(sum_of_results)

    ## create a point geometry representing the mean center
    mean_center = Point(mean_x, mean_y)

    return num_features, standard_distance, mean_center
