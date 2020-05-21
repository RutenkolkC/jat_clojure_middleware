 
/*
var data = [30, 86, 168, 281, 303, 365];

var maxdata = Math.max(...data);


console.log(maxdata)

d3.select(".chart")
  .selectAll("div")
  .data(data)
    .enter()
    .append("div")
    .style("width", function(d) { return  d / maxdata * 10 + "rem"; })
    .text(function(d) { return d; });
    
    */
var nodes = ["ClassA", "ClassB", "ClassC", "ReallyReallyLongClassNameYesThisScales"];
var edges = [["ClassA", "ClassB"],["ClassB","ClassC"],["ClassC","ClassA"],["ClassA", "ReallyReallyLongClassNameYesThisScales"]]


//$.get( "localhost:8080/package/de.prob.statespace", function( data ) {
//  alert( "Data Loaded:" + data );
//}).fail(function(data){alert("error:" + JSON.stringify(data))});
//
//$.get(
    //"http://localhost:8080/package/de.prob",
    
    //"http://localhost:8080/package/de.prob.statespace",


/*
d3.select(".chart2")
  .selectAll("div")
  .data(nodes)
    .enter()
    .append("div")
    .attr("id",id=>"node_"+id+"c2")
    .attr("class","node_div_chart2")
    .text(classname => classname)
    .on("mouseover",hoverOn)
    .on("mouseout",hoverOff)
    */
    
function hoverOn(d,i){
  d3.select(this).style("background-color","green");
}
function hoverOff(d,i){
  d3.select(this).style("background-color","steelblue");
}

    
    
d3.select(".chart3")
  .selectAll("div")
  .data(nodes)
    .enter()
    .append("div")
    .attr("id",id=>"node_"+id)
    .attr("class","node_div")
    .text(classname => classname)
    .on("mouseover",hoverOn)
    .on("mouseout",hoverOff)
    
    
var mysvgo = 
  d3.select(".dependency-svg")
    
mysvgo.attr("width",500).attr("height",200)    

mysvgo.append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", 10)
    .attr("height", 10)
    .attr("fill", "red")
  
    
const config = {
  target: $(".node_div"),
  line: $(".line"),
  delay: 40 // enter zero for live resizing
}
const drawBetweenObjects = {
  //cmake the line
  makeLine: function(line, div1, div2) {
    var className = div1.attr('id') + div2.attr('id');
      console.log(div1)
      console.log(div1.attr('id'))
      console.log(div2)
      console.log(div2.attr('id'))
      console.log(className)
    if (className.includes("undefined") !== true) { //error check
      $(line).clone().addClass('deleteMe').addClass(className).removeClass("original").insertAfter(line);
      
      var x1 = div1.offset().left + (div1.width()/2);
      var y1 = div1.offset().top + (div1.height()/2);
      var x2 = div2.offset().left + (div2.width()/2);
      var y2 = div2.offset().top + (div2.height()/2);
      var radius = (y1-y2) / 2
      
      var color = y1>y2?"red":"green"
      var x1_off = y1>y2?(div1.width()/2):-(div1.width()/2)
      var x2_off = y1>y2?(div2.width()/2):-(div2.width()/2)
      
      $("path").clone().addClass('deleteMe').addClass(className+"p").removeClass("original")
             .attr("d","M "+ (x1+x1_off) + " " + (y1) + " A " + radius + " " + radius + " 0 1 0 "+ (x2+x2_off) + " " + (y2))
             .attr("stroke",color).insertAfter(line);
      
      $("."+className).attr('x1',x1).attr('y1',y1).attr('x2',x2).attr('y2',y2); //svg attributes
    } else { console.error("undefined object.\nObject is:"+div1) }
   },
  findLines: function(search) {
   $(".deleteMe").remove(); //remove last set of lines
   
   edges.map(v=>{
     var from = v[0]
     var to = v[1]
     drawBetweenObjects.makeLine(config.line,$("#node_"+from),$("#node_"+to))
   })
   
   /*
    $(search).each(function(index, el){
      
      if ( search.eq(index + 1).length ) { //only do drawBetweenObject if there is another.
      console.log(el)
        drawBetweenObjects.makeLine(config.line, $(this), search.eq(index + 1));   //args order - line, div1 and div2 - the next div.
      }
   });*/
  },
  init: function() {
    drawBetweenObjects.findLines( $(".node_div") );
    //give resizing time to happen
    var resizeId;
    $(window).resize(function() {
        clearTimeout(resizeId);
        if (config.delay !== 0) {
          resizeId = setTimeout(doneResizing, config.delay);
        } else {
          drawBetweenObjects.findLines( $(".node_div") );
        }
    });
    function doneResizing(){
      drawBetweenObjects.findLines( $(".node_div") );
    }
  }
}

drawBetweenObjects.init();



  
  
  
  
  
  
  
  
  
