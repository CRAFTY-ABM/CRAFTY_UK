#!/bin/sh
java -Dlog4j.configuration=log4j/log4j2020_normal.properties  -XX:ActiveProcessorCount=12 -Djava.library.path=/pd/data/crafty/CRAFTY_UK/lib/jniLibs -Dfile.encoding=UTF-8 -classpath /pd/data/crafty/CRAFTY_UK/lib/CRAFTY_KIT_engine_2021_JDK15.jar:pd/data/crafty/CRAFTY_UK/lib/javacsv.jar:/pd/data/crafty/CRAFTY_UK/lib/guava-12.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib-internal/log4j-1.2.17.jar:/pd/data/crafty/CRAFTY_UK/lib-internal/stax-1.2.0.jar:/pd/data/crafty/CRAFTY_UK/lib-internal/stax-api-1.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/javaRasters.jar:/pd/data/crafty/CRAFTY_UK/lib/simple-xml-2.7.1.jar:/pd/data/crafty/CRAFTY_UK/lib/colt-1.2.0.jar:/pd/data/crafty/CRAFTY_UK/lib/Uranus.jar:/pd/data/crafty/CRAFTY_UK/lib/ParMa.jar:/pd/data/crafty/CRAFTY_UK/libbin:/pd/data/crafty/CRAFTY_UK/lib/collections-generic-4.01.jar:/pd/data/crafty/CRAFTY_UK/lib/colt-1.2.0.jar:/pd/data/crafty/CRAFTY_UK/lib/commons-cli-1.2.jar:/pd/data/crafty/CRAFTY_UK/lib/commons-math3-3.2.jar:/pd/data/crafty/CRAFTY_UK/lib/jts-1.13.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-opengis-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-metadata-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-referencing-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-api-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-main-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-data-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-epsg-wkt-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/gt-shapefile-9.0.jar:/pd/data/crafty/CRAFTY_UK/lib/guava-12.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jai_core.jar:/pd/data/crafty/CRAFTY_UK/lib/javacsv.jar:/pd/data/crafty/CRAFTY_UK/lib/javaRasters.jar:/pd/data/crafty/CRAFTY_UK/lib/jchart2d-3.2.2.jar:/pd/data/crafty/CRAFTY_UK/lib/jep_ext-1.1.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jep-2.4.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jide-oss-2.9.7.jar:/pd/data/crafty/CRAFTY_UK/lib/JRI.jar:/pd/data/crafty/CRAFTY_UK/lib/jscience.jar:/pd/data/crafty/CRAFTY_UK/lib/jung-api-2.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jung-algorithms-2.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jung-graph-impl-2.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/wstx-asl-3.2.6.jar:/pd/data/crafty/CRAFTY_UK/lib/stax-api-1.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/jung-io-2.0.1.jar:/pd/data/crafty/CRAFTY_UK/lib/LARA_Base.jar:/pd/data/crafty/CRAFTY_UK/lib/LARA_Toolbox.jar:/pd/data/crafty/CRAFTY_UK/lib/log4j-1.2.17.jar:/pd/data/crafty/CRAFTY_UK/lib/monte-cc.jar:/pd/data/crafty/CRAFTY_UK/lib/MORe.jar:/pd/data/crafty/CRAFTY_UK/lib/ParMa.jar:/pd/data/crafty/CRAFTY_UK/lib/repast.simphony.bin_and_src.jar:/pd/data/crafty/CRAFTY_UK/lib/saf.core.runtime.jar:/pd/data/crafty/CRAFTY_UK/lib/simple-xml-2.7.1.jar:/pd/data/crafty/CRAFTY_UK/lib/units-0.01.jar:/pd/data/crafty/CRAFTY_UK/lib/Uranus.jar:/pd/data/crafty/CRAFTY_UK/lib/vecmath-1.3.1.jar:/pd/data/crafty/CRAFTY_UK/lib/xmlgraphics-commons-1.3.1.jar:/pd/data/crafty/CRAFTY_UK/lib/mpi.jar org.volante.abm.serialization.ModelRunner -d /pd/data/crafty/CRAFTY_UK/data_UK -f Scenario_UK_Baseline-SSP3_everyyear_relative_landusecontrol_thresholds_noGUI.xml -o 99 -r 1 -n 1 -sr 0
