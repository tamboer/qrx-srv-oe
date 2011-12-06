#########################################################################################
###                                                                                   ###
###                       Framework Configuration Folder                              ###
###                                                                                   ###
#########################################################################################

This folder is used by the default configuration service of the framework if xml
configuration is preferred over the direct configuration in the run-time database.

For each hosted application the framework will lookup for a xml configuration file
in that directory using the following naming convention:

[lower(application name)].xml

The configuration file format must respect the xml structure:

<config>
	<section name={section name}>
		<key name={key name} [object={object name}]>{key value}</key>
		[key...]
	</section>
	[section...]
</config>