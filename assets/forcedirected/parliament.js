class Parliament{
    constructor( svg_reference , config ) {
        this.target = svg_reference;
        this.layout = new GridBoxLayout();
        this.fillFunction = (d)=>"green";
        this.items = 0;
        this.config = {
            squareWidth:8,
            squareHeight:8,
            squareSpacingX:12,
            squareSpacingY:12,
            delayMs:1,
            barSeparationFaction: 1.4,
            barTargetAspectRation: 0.2,
            scatterBlockWidth: 50,
            scatterBlockHeight:50,
            width:500,
            height:550,
            margin_left:60,
            margin_right:60,
            margin_top:60,
            margin_bottom:60,
            axis_offset:40,
            extraAxisPadding:0.1,
            ...config
        };

        this.scales = {
            xScale:d3.scaleLinear()
                   .domain([0, 2])
                   .range([0,0]),
            yScale:d3.scaleLinear()
                    .domain([0, 2])
                    .range([0,0])
        };
        this.scales.xAxisGenerator = d3.axisBottom(this.scales.xScale);
        this.scales.yAxisGenerator = d3.axisLeft(this.scales.yScale);

        this.squares_g = this.target.append("g")
            .attr("class","squares");
        
        this.scales_g = this.target.append("g")
            .attr("class","scales");
        this.scales_x = this.scales_g.append("g")
            .attr("class","scale_x");
        this.scales_y = this.scales_g.append("g")
            .attr("class","scale_y");
        this.label_x = this.scales_g.append("text")
            .attr("class","label_x")
            .text("x_axis");
        this.label_y = this.scales_g.append("text")
            .attr("class","label_y")
            .text("y_axis");
    }
    setData( data ){
        this.data = data;
    }
    setLayout( layout ){
        this.layout = layout;
    }
    setColor( fillFunction ){
        this.fillFunction = fillFunction;
    }
    updateConfig(){
        this.config.inner_width = this.config.width - this.config.margin_left - this.config.margin_right;
        this.config.inner_height = this.config.height - this.config.margin_top - this.config.margin_bottom;
        this.config.mid_x = this.config.inner_width/2 + this.config.margin_left;
        this.config.mid_y = this.config.inner_height/2 + this.config.margin_top;
        this.config.position_x = (x)=>this.config.margin_left + x * this.config.inner_width;
        this.config.position_y = (y)=>this.config.margin_top + y * this.config.inner_height;
        this.config.halfSquareWidth = this.config.squareWidth / 2;
        this.config.halfSquareHeight = this.config.squareHeight / 2;
    }
    jump(){
        this.updateConfig();
        const config = this.config;
        this.layout.layout( this.data , config , this.scales );
        this.calculateTargetZoom();
        this.calculateScaleTransform();
        this.squares_g.attr("transform",this.scaleTransforms.body );
        this.squares_g.selectAll("rect.parliament").interrupt();
        this.squares_g
            .selectAll("rect.parliament")
            .data(this.data)
        .join(
            enter => enter.append("rect")
              .attr("class","parliament")
              .attr("fill", this.fillFunction)
              .attr("width", config.squareWidth)
              .attr("height", config.squareHeight)
              .attr("x", d=>this.fixNaN(d.x))
              .attr("y", d=>this.fixNaN(d.y))
              .attr("title",d=>JSON.stringify(d)),
            update => update
                .attr("x", d=>this.fixNaN(d.x))
                .attr("y", d=>this.fixNaN(d.y))
                .attr("width", config.squareWidth)
                .attr("height", config.squareHeight),
            exit => exit
              .remove());
        this.scales_x
            .call(this.scales.xAxisGenerator)
            .attr("transform",this.scaleTransforms.scaleX)
            .attr("opacity",this.layout.getXOpacity());
        if ( this.layout.getXCategorical() ){
            this.scales_x.selectAll(".tick text")
                .style("text-anchor", "start");
        }else{
            this.scales_x.selectAll(".tick text")
                .style("text-anchor", null);
        }
        this.scales_y
            .call(this.scales.yAxisGenerator)
            .attr("transform",this.scaleTransforms.scaleY )
            .attr("opacity",this.layout.getYOpacity());
        this.label_x
            .text(this.layout.getXTitle())
            .attr("transform",`translate( ${config.margin_top+config.inner_width/2} ${config.margin_top+config.inner_height+config.axis_offset})`)
            .attr("opacity",this.layout.getXOpacity());
        this.label_y
            .text(this.layout.getYTitle())
            .attr("transform",`translate(${config.margin_left- config.axis_offset} ${config.margin_top + config.inner_height/2 })  rotate(90)`)
            .attr("opacity",this.layout.getYOpacity());
    }
    transition(durationMS){
        this.updateConfig();
        const config = this.config;
        this.layout.layout( this.data , config , this.scales );
        this.calculateTargetZoom();
        this.calculateScaleTransform();
        this.squares_g 
            .transition().duration(durationMS)
            .ease(d3.easeCubicOut)
            .attr("transform", this.scaleTransforms.body );
        this.squares_g
            .selectAll("rect.parliament")
            .data(this.data)
        .join(
            enter => enter.append("rect")
              .attr("class","parliament")
              .attr("fill", this.fillFunction)
              .attr("width", 0)
              .attr("height", 0)
              .attr("x", d=>this.fixNaN(d.x))
              .attr("y", d=>this.fixNaN(d.y)),
            update => update,
            exit => exit
              .remove());
        this.squares_g
            .selectAll("rect.parliament")
            .interrupt()
            .transition().duration(durationMS).delay( (d,i)=> (config.delayMs)*i )
              .attr("width", config.squareWidth)
              .attr("height", config.squareHeight)
              .attr("fill", this.fillFunction)
              .attr("x", d=>this.fixNaN(d.x))
              .attr("y", d=>this.fixNaN(d.y));
        this.scales_x.transition().duration(durationMS)
            .call(this.scales.xAxisGenerator)
            .attr("transform",this.scaleTransforms.scaleX)
            .attr("opacity",this.layout.getXOpacity());
        if ( this.layout.getXCategorical() ){
            this.scales_x.selectAll(".tick text")
                .style("text-anchor", "start");
        }else{
            this.scales_x.selectAll(".tick text")
                .style("text-anchor", null);
        }
        this.scales_y.transition().duration(durationMS)
            .call(this.scales.yAxisGenerator)
            .attr("transform",this.scaleTransforms.scaleY )
            .attr("opacity",this.layout.getYOpacity());
        this.label_x.transition().duration(durationMS)
            .text(this.layout.getXTitle())
            .attr("transform",`translate( ${config.margin_top+config.inner_width/2} ${config.margin_top+config.inner_height+config.axis_offset})`)
            .attr("opacity",this.layout.getXOpacity());
        this.label_y.transition().duration(durationMS)
            .text(this.layout.getYTitle())
            .attr("transform",`translate(${config.margin_left- config.axis_offset} ${config.margin_top + config.inner_height/2 })  rotate(90)`)
            .attr("opacity",this.layout.getYOpacity());
    }
    fixNaN(v){
      if ( Number.isNaN(v) ){
        return -1;
      }else{
        return v;
      }
    }
    calculateTargetZoom(){
        const bounds = { x:{} , y:{} };
        function expandBounds( b , v ){
            if (b.min == undefined || b.min > v ){
                b.min = v;
            }
            if (b.max == undefined || b.max < v ){
                b.max = v;
            }
        }
        this.data.forEach(element => {
            expandBounds( bounds.x , element.x );
            expandBounds( bounds.y , element.y );
        });
        if ( this.layout.getYOpacity() ){
            bounds.x.min -= (bounds.x.max-bounds.x.min)*this.config.extraAxisPadding;
        }
        if ( this.layout.getXOpacity() ){
            bounds.y.max = (bounds.y.max-bounds.y.min)*this.config.extraAxisPadding;
        }
        this.dataBounds = bounds;
    }
    calculateScaleTransform(){
        const boundsWidth = this.dataBounds.x.max - this.dataBounds.x.min + this.config.squareWidth;
        const boundsHeight = this.dataBounds.y.max - this.dataBounds.y.min + this.config.squareHeight;
        const scaleX = this.config.inner_width / boundsWidth;
        const scaleY = this.config.inner_height / boundsHeight;
        const scale = Math.min(scaleX,scaleY); // choose the smallest one as we prefer margins to overlaps
        if ( Number.isNaN(scale) ) return "";
        const finalWidth = scale * boundsWidth;
        const finalHeight = scale * boundsHeight;
        const shiftX = this.config.margin_left + (this.config.inner_width - finalWidth) / 2;
        const shiftY = this.config.margin_top + (this.config.inner_height - finalHeight) / 2;
        this.scaleTransforms = {
            body:`translate(${shiftX} ${shiftY}) scale(${scale} ${scale}) translate(${-this.dataBounds.x.min} ${-this.dataBounds.y.min})`,
            scaleX:`translate(${shiftX} ${shiftY}) scale(${scale} ${scale}) translate(${-this.dataBounds.x.min} ${-this.dataBounds.y.min})`,
            scaleY:`translate(${shiftX} ${shiftY}) scale(${scale} ${scale}) translate(${-this.dataBounds.x.min} ${-this.dataBounds.y.min})`,
        }
    }
}

function distanceBetween(a,b){
    if ( a == undefined || b == undefined ){
        throw new Error("Undefined position in distance function");
    }
    const dx = (a.x||0) - (b.x||0);
    const dy = (a.y||0) - (b.y||0);
    return Math.sqrt( dx*dx + dy*dy );
}

class GridBoxLayout{
    constructor(){
        this.sortFunction = (d)=>0;
    }
    sortBy( sortFunction ){
        this.sortFunction = sortFunction;
    }
    getXOpacity(){
        return 0.0;
    }
    getYOpacity(){
        return 0.0;
    }
    getXCategorical(){
        return false;
    }
    getYTitle(){
        return "";
    }
    getXTitle(){
        return "";
    }
    layout(data,config,scales){
        const targets = this.layoutPositions(data,config,scales); // get the target positions
        const groupIndexes = {};
        for ( let j=0;j<targets.length;j++){
            const g = targets[j].g;
            if ( ! groupIndexes[g] ){
                groupIndexes[g] = [j];
            }else{
                groupIndexes[g].push(j);
            }
        }
        const sourceGroupIndexes = {};
        for ( let i=0;i<data.length;i++){
            const g = this.sortFunction(data[i]);
            if ( ! sourceGroupIndexes[g] ){
                sourceGroupIndexes[g] = [i];
            }else{
                sourceGroupIndexes[g].push(i);
            }
        }
        for ( let g in groupIndexes ){
            const sourceIndexes = sourceGroupIndexes[g];
            const targetIndexes = groupIndexes[g];
            this.assignPositions( sourceIndexes , targetIndexes , data , targets );
        }
    }
    assignPositions( sourceIndexes , targetIndexes , data , targets ){
        const distances = {};
        // compute the distance matrix
        for (let i=0;i<sourceIndexes.length;i++){
            const data_index = sourceIndexes[i];
            const d = data[data_index];
            const row = {};
            distances[data_index]=row;
            for (let j=0;j<targetIndexes.length;j++){
                const target_index = targetIndexes[j];
                const t = targets[target_index];
                row[target_index] = distanceBetween( t , d); 
            }
        }
        while ( Object.keys(distances).length > 0 ){
            const closest = this.findMinimum( distances );
            const closest_i = closest[0];
            const closest_j = closest[1];
            delete distances[closest_i];
            for ( let n in distances ){
                delete distances[n][closest_j];
            }
            const d = data[closest_i];
            const t = targets[closest_j];
            d.x = t.x;
            d.y = t.y;
        }
    }
    findMinimum( matrix ){
        let bestPair = [-1,-1];
        let bestValue = Number.MAX_VALUE;
        for ( let i in matrix ){
            const row = matrix[i];
            for ( let j in row ){
                const value = row[j];
                if ( value < bestValue ){
                    bestValue = value;
                    bestPair = [i,j];
                }
            }
        }
        return bestPair;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        var sortF = undefined;
        if ( typeof(groups[0]) == "number"){
            sortF = (a,b)=>a-b;
        }
        const sortedGroups = groups.sort( sortF );
        return sortedGroups;
    }
    layoutPositions(data,config){
        const items = data.length;
        const sides = Math.floor(Math.sqrt(items));
        const half_side = sides/2;
        const sortedGroups = this.getSortedGroups( items ,data );
        // Here's the ma
        const positions = [];
        for (let i=0;i<items;i++){
            const row = i % sides;
            const col = (i -row)/sides;
            const x = (row-half_side) * config.squareSpacingX;
            const y = (col-half_side) * config.squareSpacingY;
            positions.push( { x:x, y:y, g:sortedGroups[i]} );
        }
        return positions;
    }
}

class SemicircleLayout extends GridBoxLayout{
    constructor( config ){
        super();
        this.config = {
            inner_radius:40,
            arc_start_radians:-1,
            arc_end_radians:1,
            ...config
        }
    }
    layoutPositions(data,config){
        const items = data.length;
        // Here's the math's happens to fit them in the shape..
        const sortedGroups = this.getSortedGroups( items , data );
        const positions = [];
        let row = 0;
        let col = 0;
        let arc_total = this.config.arc_end_radians - this.config.arc_start_radians;

        for (let i=0;i<items;i++){
            const radius = (row*config.squareSpacingY)+this.config.inner_radius;
            const col_length = (arc_total * radius) / config.squareSpacingX;
            if ( (col+1) > col_length ){
                col = 0;
                row ++;
            }else{
                col ++;
            }
            const angle = ((col / col_length) * arc_total)+this.config.arc_start_radians;
            const x = Math.sin(angle) * radius;
            const y = -Math.cos(angle) * radius;
            positions.push( { x:x, y:y , theta:angle} );
        }
        positions.sort( (a,b)=>a.theta-b.theta );
        for (let i=0;i<items;i++){
            positions[i].g = sortedGroups[i];
        }
        return positions;
    }
}

class BarchartLayout extends GridBoxLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getXTitle(){
        return this.sortFunction.title||"?";
    }
    layoutPositions(data,config,scales){
        //this.barWidthPixels = config.barWidth * config.squareSpacingX + config.barSeparation;
        const counts = {};
        const groups = [];
        const group_positions = [];
        let max_group_size = 0;
        for (let id in data ){
            const d = data[id];
            const g = this.sortFunction(d);
            if( ! counts[g] ){
                counts[g] = 0;
                groups.push(g);
            }
            counts[g] = counts[g]+1;
            if ( counts[g] > max_group_size ){
                max_group_size = counts[g];
            }
        }
        // now we know how many groups we have, and the size of each,
        // sort groups be counts?

        const sortedGroups = groups.map(v=>[v, counts[v]]).sort( (a,b)=>a[1]-b[1] ).map( (a)=>a[0 ]);

        const targetRatio = config.barTargetAspectRation * groups.length * config.barSeparationFaction;
        const barWidth = Math.ceil(Math.sqrt(max_group_size / targetRatio));
        console.log( targetRatio , max_group_size , barWidth );
        const group_count = groups.length/2;
        for ( let index in groups ){
            group_positions[index] = (barWidth *config.squareSpacingX * config.barSeparationFaction) * (index - (group_count / 2));
        }

        
        scales.xScale = d3.scaleOrdinal()
            .domain(sortedGroups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        const items = data.length;
        const positions = [];
        for (let i=0;i<items;i++){
            const d = data[i];
            const bar = this.sortFunction(d);
            const height = counts[bar] -1; // This is the hight of this block...
            const barX = (height % barWidth) - (barWidth/2);
            const barY = 1 + ((height - barX)/barWidth);
            counts[bar] --;
            const y = - barY *  config.squareSpacingY;
            const x = scales.xScale(bar) + barX*config.squareSpacingX;
            positions.push( { x:x, y:y, g:bar} );
        }
        return positions;
    }
}


class ScatterchartLayout extends GridBoxLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getYOpacity(){
        return 1.0;
    }
    getXTitle(){
        return this.sortFunction.Xtitle||"?";
    }
    getYTitle(){
        return this.sortFunction.Ytitle||"?";
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items , data);
        let minX= undefined;
        let minY= undefined;
        let maxX= undefined;
        let maxY= undefined;
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = parseFloat(parts[0]);
            const y_value = parseFloat(parts[1]);
            if ( x_value < minX || minX===undefined) minX = x_value;
            if ( x_value > maxX || maxX===undefined) maxX = x_value;
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        const width = config.scatterBlockWidth *config.squareSpacingX;
        const height = config.scatterBlockHeight *config.squareSpacingY;
        scales.xScale = d3.scaleLinear()
               .domain([minX, maxX])
               .range([0,width]);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        scales.yScale = d3.scaleLinear()
                .domain([minY, maxY])
               .range([0, -height]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = parseFloat(parts[0]);
            const y_value = parseFloat(parts[1]);
            positions.push( { x:scales.xScale(x_value), y:scales.yScale(y_value)-config.squareHeight, g:group } );
        }
        return positions;
    }
}

class ScatterchartXCategoricalLayout extends ScatterchartLayout{
    constructor(){
        super();
    }
    getXCategorical(){
        return true;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items ,data);
        let minY= undefined;
        let maxY= undefined;
        const groups = [];
        for (let i in sortedGroups){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const g = parts[0];
            if( groups.indexOf(g) == -1 ){
                groups.push(g);
            }
        }
        const group_positions = [];
        const group_width = (config.inner_width / groups.length);
        for (let i in groups){
            group_positions[i] = config.margin_left + group_width*(parseFloat(i)+0.5);
        }
        scales.xScale = d3.scaleOrdinal()
            .domain(groups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const y_value = parseFloat(parts[1]);
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        scales.yScale = d3.scaleLinear()
               .domain([minY, maxY])
               .range([config.margin_top+config.inner_height,config.margin_top]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = scales.xScale(parts[0]) + ((Math.random()-0.5)*(Math.random()-0.5))*group_width;
            const y_value = parseFloat(parts[1]);
            positions.push( { x:x_value-config.halfSquareWidth, y:scales.yScale(y_value)-config.halfSquareHeight, g:group } );
        }
        return positions;
    }
}

class ScatterchartXCategoricalGridLayout extends ScatterchartLayout{
    constructor(){
        super();
    }
    getXCategorical(){
        return true;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items , data );
        let minY= undefined;
        let maxY= undefined;
        const groups = [];
        for (let i in sortedGroups){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const g = parts[0];
            if( groups.indexOf(g) == -1 ){
                groups.push(g);
            }
        }
        const height = config.scatterBlockHeight *config.squareSpacingY;
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const y_value = parseFloat(parts[1]);
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        scales.yScale = d3.scaleLinear()
               .domain([minY, maxY])
               .range([0,-height]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        const counters1 = {};
        let max_bar_width = 0;
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const base_y_value = scales.yScale(parseFloat(parts[1]));
            const offset_y = base_y_value % config.squareSpacingY;
            const plotY = base_y_value - offset_y;
            const key = parts[0]+","+plotY;
            if ( counters1[key] === undefined ){
                counters1[key] = 0;
            }else{
                counters1[key]++;
            }
            if ( counters1[key] > max_bar_width ){
                max_bar_width = counters1[key];
            }
        }
        const group_positions = [];
        for (let i in groups){
            group_positions[i] = max_bar_width*parseFloat(i) * config.squareSpacingX * config.barSeparationFaction;
        }
        scales.xScale = d3.scaleOrdinal()
            .domain(groups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);

        const counters2 = {};
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const base_y_value = scales.yScale(parseFloat(parts[1]));
            const offset_y = base_y_value % config.squareSpacingY;
            const plotY = base_y_value - offset_y;
            const key = parts[0]+","+plotY;
            if ( counters2[key] === undefined ){
                counters2[key] = 0;
            }else{
                counters2[key]++;
            }
            const x_offset = counters2[key] * config.squareSpacingX;
            const x_value = scales.xScale(parts[0]) + x_offset;
            positions.push( { x:x_value, y:plotY-config.squareSpacingY, g:group } );
        }
        return positions;
    }
}

class ParliamentControl extends Parliament {
    constructor( svg_element , div_element, config ) {
        super(svg_element, config);
        this.control = {
            target:div_element,
            fields:{},
            sortFunctions:[],
            layouts:[]
        };

        // add resizer here
        if ( config.autoSize ){
            this.resizer = new ParliamentResizer( this.target.node().parentNode , this );
        }

        // now we create some specific known fields:
        // a gender specific colour function
        function genderColor(d){
            if (d.gender=="m") return "blue";
            if (d.gender=="f") return "pink";
            return "grey";
        }
        // add this to the known fields with overrides
        this.control.fields.gender = {
            color:genderColor,
            title:"Gender (m/f/u)"
        };

        // now we create the layout versions:
        this.control.layouts.push( {
            title:"Parliament",
            key:'p',
            sortOptions:[ { filter:(d)=>true } ],
            layout:new SemicircleLayout({
                inner_radius:50,
                arc_start_radians:-2,
                arc_end_radians:2,
                }) 
        });

        this.control.layouts.push( {
            title:"Box",
            key:'b',
            sortOptions:[ { filter:(d)=>true } ],
            layout:new GridBoxLayout()
        });

        this.control.layouts.push( {
            title:"Barchart",
            key:'bar',
            sortOptions:[ { filter:(d)=>!d.scalar } ],
            layout:new BarchartLayout()
        });

        this.control.layouts.push( {
            title:"ScatterChart",
            key:'sc1',
            sortOptions:[ { filter:(d)=>d.scalar }, { filter:(d)=>d.scalar } ],
            layout:new ScatterchartLayout()
        });
        this.control.layouts.push( {
            title:"ScatterChart Categorical",
            key:'sc2',
            sortOptions:[ { filter:(d)=>!d.scalar }, { filter:(d)=>d.scalar } ],
            layout:new ScatterchartXCategoricalGridLayout()
        });

        // this creates the form
        this.populateFormDetails();
        this.layoutChanged( true );
    }
    setData(data){
        super.setData(data);
        if ( data.length > 0 ){
            const example = data[0];
            const keys = Object.keys(example);
            this.deleteAll(this.control.sortFunctions);
            for ( let index in keys ){
                const key  = keys[index];
                const obj = { key:key, title:key };

                if (this.control.fields[key]){
                    this.copyInto(this.control.fields[key],obj);
                }
                if ( obj.scalar === undefined ){
                    obj.scalar = ( typeof example[key] == "number" );
                }
                if ( obj.color === undefined ){
                    if ( obj.scalar ){
                        obj.range = this.computeRange( data, (d)=>d[key]);
                        // default colour scale for Scalar data
                        obj.colorScale = d3.scaleSequential(d3.interpolateViridis);
                        obj.color = (d)=>obj.colorScale(obj.range.scale(d[key]));
                    }else{
                        // default colour scale for Categorical data
                        obj.colorScale = d3.scaleOrdinal(d3.schemeAccent);
                        obj.color = (d)=>obj.colorScale(d[key]);
                    }
                }
                if ( obj.sorter === undefined ){
                    obj.sorter = (d)=>d[key];
                }
                if ( obj.sorter.title === undefined ){
                    obj.sorter.title = key;
                }

                this.control.sortFunctions.push( obj );
            }
            this.control.orderA.update();
            this.control.orderB.update();
            this.control.colorDropdown.update();
        }
        this.colorChanged();
        this.layoutChanged();
    }
    deleteAll(array){
        array.splice(0,array.length);
    }
    copyInto(src,target){
        const keys = Object.keys(src);
        for ( let index in keys ){
            target[keys[index]] = src[keys[index]];
        }
    }
    computeRange(data,mapper=(d)=>d){
        const o = {
            min:Number.MAX_VALUE,
            max:Number.MIN_VALUE,
        };
        for ( let index in data ){
            const v = mapper(data[index]);
            if ( v > o.max ) o.max = v;
            if ( v < o.min ) o.min = v;
        }
        o.range = o.max - o.min;
        o.scale = (d)=>(d-o.min)/o.range;
        return o;
    }
    populateFormDetails(){
        // firstly the layout dropdown.
        this.control.layoutDropdown = this.buildDropdown( "layout" , "Layout" , this.control.layouts , ()=>this.layoutChanged() );
        this.control.orderA = this.buildDropdown( "orderA" , "Order", this.control.sortFunctions , ()=>this.sortChanged() );
        this.control.orderB = this.buildDropdown( "orderB" , null, this.control.sortFunctions , ()=>this.sortChanged() );
        this.control.colorDropdown = this.buildDropdown( "color" , "Color", this.control.sortFunctions , ()=>this.colorChanged());
    }
    buildDropdown( title , label , target , change){
        if ( label != null ){
            this.control.target
            .append('label')
            .text( label )
            .attr("for", `${title}-list-id`);
        }
        const elem = this.control.target
            .append('select')
            .attr("id", `${title}-list-id`)
            .attr("name", `${title}-list`)
            .on("change",change);
        const obj = {
            title:title,
            elem:elem,
            filter:(d)=>true
        };
        function updateF(){
            elem.selectAll('option').data( target ).join(
                enter => enter.append("option")
                  .text(d=>d.title)
                  .attr("value",d=>d.key)
                  .attr("disabled",d=>(obj.filter(d))?null:"true"),
                update => update
                  .text(d=>d.title)
                  .attr("value",d=>d.key)
                  .attr("disabled",d=>(obj.filter(d))?null:"true"),
                exit => exit
                  .remove());
            
        }
        function getF( filter=(d)=>true){
            const value = elem.node().value;
            for ( let index in target ){
                if ( filter(target[index]) && target[index].key == value ){
                    return target[index];
                }
            }
            return null;
        }
        function setEnabledF(t){
            elem.attr("disabled",t?null:"true");
        }
        updateF();
        obj.update = updateF;
        obj.get = getF;
        obj.setEnabled = setEnabledF;
        return obj;
    }
    layoutChanged( ){
        const layout = this.control.layoutDropdown.get();
        //console.log("Layout changed",layout);
        if ( layout && this.data ){
            if ( layout.key != this.activelayout){
                this.activelayout = layout.key; // don't change id we're already done
                this.activeLayoutDetails = layout;
                try{
                    this.control.orderA.filter = layout.sortOptions[0].filter;
                    this.control.orderA.update();
                    if (layout.sortOptions[1] ){
                        this.control.orderB.filter = layout.sortOptions[1].filter;
                        this.control.orderB.update();
                        this.control.orderB.setEnabled(true);
                        this.sorters = 2;
                    }else{
                        this.control.orderB.setEnabled(false);
                        this.sorters = 1;
                    }
                    this.setLayout( layout.layout );
                    if ( this.applySort() ){
                        this.transition(1000);
                    }
                }catch( e ){
                    console.error(e);
                }

            }
        }
    }
    sortChanged(){
        //console.log("Sort changed");
        if ( this.applySort() ){
            this.transition(1000);
        }
    }
    applySort(){
        if ( this.data ){
            const sorterA = this.control.orderA.get( this.activeLayoutDetails.sortOptions[0].filter );
            if ( this.sorters == 1 ){
                if ( !sorterA  ) return false;
                this.layout.sortBy( sorterA.sorter );
            }else{
                const sorterB = this.control.orderB.get( this.activeLayoutDetails.sortOptions[1].filter  );
                if ( !sorterA || !sorterB ) return false;
                this.layout.sortBy( this.combineSorters(sorterA.sorter,sorterB.sorter) );
            }
            return true;
        }
    }
    combineSorters( sa , sb ){
        const f = (d)=>sa(d)+","+sb(d);
        f.Xtitle = sa.title;
        f.Ytitle = sb.title;
        f.title = sa.title+","+sb.title;
        return f;
    }
    colorChanged(){
        const sorter = this.control.colorDropdown.get();
        if ( sorter && this.data ){
            this.setColor( sorter.color );
            this.transition(100,25);
        }
    }
}

// This class automatically resizes the parliment display when it's container changes size
class ParliamentResizer {

    constructor( wrapper_div_element , parliament ){
        this.wrapper_div_element = wrapper_div_element;
        this.parliament = parliament;

        new ResizeObserver( ()=>this.triggerResize() ).observe(wrapper_div_element);
    }

    triggerResize(){
        const width = Math.max(10,this.wrapper_div_element.offsetWidth - 4);
        const height = Math.max(this.wrapper_div_element.offsetHeight - 4);
        this.parliament.config.width = width;
        this.parliament.config.height = height;
        this.parliament.updateConfig();
        this.parliament.target.attr("width",width+"px");
        this.parliament.target.attr("height",height+"px");
        this.parliament.jump();
    }
}