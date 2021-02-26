require 'octokit'
require 'json'

client = Octokit::Client.new(:auth_token => '77d74a2d71af81c0643efe70a4ad3d286608fc29')
client.auto_paginate = true

event = JSON.parse( File.read("C:\\Users\\Joe\\milestone_event.json") )
milestone = {number: event["milestone"]["number"], title: event["milestone"]["title"]} 

now = Time.now
news = Array.new

client.list_issues("westes/flex", :milestone => milestone[:number], :state => "closed").each do |issue|
  news.push "##{issue.number}: #{issue.title} (#{issue.milestone.title}) [#{issue.labels.reduce(" ") {|r, label| r + label.name + " "}}]"
end

infile = File.open("D:\\Users\\Joe\\Documents\\GitHub\\flex\\NEWS")
outfile = File.open("D:\\Users\\Joe\\Documents\\GitHub\\flex\\NEWS.new", "w")

outfile.write infile.gets

outfile.write "\n"
outfile.write "* Noteworthy changes in release #{milestone[:title]} (#{now.year}-#{now.month}-#{now.day})\n"
outfile.write "\n"
news.each {|n| outfile.write "#{n}\n"}

infile.each {|l| outfile.write l}

infile.close
outfile.close
