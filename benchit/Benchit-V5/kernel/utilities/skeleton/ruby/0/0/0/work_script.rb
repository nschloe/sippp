require 'getoptlong'
require 'dbi'

def grepArgv

	opts = GetoptLong.new(
		['--BIMode',        GetoptLong::REQUIRED_ARGUMENT],
		['--BIProblemSize', GetoptLong::REQUIRED_ARGUMENT] )
	
	$PARAMETERS = {}

	# read the parameters into parameter hash	
	opts.each_option do |name,arg|
 	  $PARAMETERS[ name[2..-1] ] = arg;
	end
end

def measurementOverhead
  puts "nothing yet\n"
end

def measurementInit
  # doing some work to initialize measurement if needed
end

def measurementRead
  # doing the measurement
end

grepArgv

case $PARAMETERS['BIMode']
	when 'overhead'
	  measurementOverhead
	when 'init'
	  measurementInit
	when 'read'
	  measurementRead
	else
	  puts "Invalid measurementmode supplied!\n"
end
