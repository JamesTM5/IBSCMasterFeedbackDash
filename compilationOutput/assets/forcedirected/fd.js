
class ForceDirected {
    constructor(el) {
      console.log("Created ForceDirected");
      this.el = el
      this.svg = d3.select(el).append("svg")
        .attr("width", 1000)
        .attr("height", 600)
        .attr("viewBox", "-100 -200 500 550")
      // group the contents of the graph separate from the svg itself to enable panning/zooming  
      this.top_group = this.svg.append("g")
  
      //add zoom function
      this.svg.call(d3.zoom().on("zoom",
        event => this.top_group.attr("transform", event.transform)
      ))
  
      this.edgeGroup = this.top_group.append("g").attr("class", "edges")
      this.nodeGroup = this.top_group.append("g").attr("class", "nodes")
  
      this.current = {
        nodes: [],
        nodesById: {},
        edges: []
      }
  
      //set colour scheme
      this.series = d3.scaleOrdinal(d3.schemeCategory10)
  
      //set up the simulation
      this.simulation = d3.forceSimulation([])
        .force("charge", d3.forceManyBody().strength(-90))
        .force("link", d3.forceLink([]).distance(120))
        .force("center", d3.forceCenter())
        .force("collision", d3.forceCollide(d => d.r))
        .on("tick",()=>this.updatePositions());
  
      this.input = {
        nodes: [],
        edges: []
      }
  
      this.styleFunctions = {
        edge_colour: (d) => "darkgrey",
        edge_width: (d) => 3.0,
        node_radius: (d) => 10.0,
        node_community: (d) => d?.community,
        node_stroke: (d) => "lightgrey"
      };
  
      this.selections = {
        circle: null,
        edge: null,
        text: null
      }
    }
  
    loadData(el, values) {
      this.redrawGraph(values.nodes, values.edges, true);
    }
  
    redrawGraph(nodes, edges, update_alpha = false) {
      const currentNodesById = {}
      for (let index in this.current.nodes) {
        const node = this.current.nodes[index]
        currentNodesById[node.id] = node
      }
  
      if (nodes) this.input.nodes = nodes;
      if (edges) this.input.edges = edges;
      this.current.nodes = this.input.nodes.map(d => {
        return {
          id: d.id,
          r: this.styleFunctions.node_radius(d),
          g: this.styleFunctions.node_community(d) || currentNodesById[d.id]?.g || "?", // if node_community returns null, keep the current
          s: this.styleFunctions.node_stroke(d),
          x: currentNodesById[d.id]?.x,
          y: currentNodesById[d.id]?.y
        }
      });
      this.current.edges = this.input.edges.map(d => {
        return {
          source: d.source,
          target: d.target,
          width: this.styleFunctions.edge_width(d),
          colour: this.styleFunctions.edge_colour(d),
        }
      });
      this.simulation.nodes(this.current.nodes)
      this.simulation.force("link")
        .id(node => node.id)
        .links(this.current.edges)

      if (update_alpha) {
        this.simulation.alphaDecay(.01)
        this.simulation.velocityDecay(0.2)
        this.simulation.alpha(0.3).restart()
      }
      this.updateAll();
    }
  
    updateAll() {
      this.selections.circle = this.nodeGroup.selectAll("circle")
      this.selections.circle.data(this.current.nodes, d => d.id).join(
        enter => enter.append("circle")
          .attr("fill", d => this.series(d.g))
          .attr("stroke", d =>d.s)
          //.attr("stroke", d=>series(d.s)) //removed to enable default no edge.
          .attr("stroke-width", 3)
          .transition()
          .duration(1500)
          .ease(d3.easeBounce)
          .attr("r", (d) => d.r),
        update => update
          .transition()
          .duration(1000)
          .ease(d3.easeBounce)
          .attr("fill", d => this.series(d.g))
          .attr("stroke", d => d.s)
          .attr("r", (d) => d.r),
        exit => exit.remove())
      this.selections.circle = this.nodeGroup.selectAll("circle")
  
      //join labels to nodes
      this.selections.text = this.nodeGroup.selectAll("text")
      this.selections.text.data(this.current.nodes, d => d.id).join(
        enter => enter.append("text")
          .text(d => d.id)
          .attr("font-size", "12px")
          .attr("x", d => d.x)
          .attr("y", d => d.y)
          .attr("text-anchor", "middle")
          .attr("transform", "translate(0, -15)")
          .transition()
          .duration(2000)
          .attr("opacity", 1),
        update => update,
        exit => exit.remove())
      this.selections.text = this.nodeGroup.selectAll("text")
  
      // join data for edges
      this.selections.edge = this.edgeGroup.selectAll("line")
      this.selections.edge.data(this.current.edges).join(
        enter => enter.append("line")
          .attr("opacity", 0)
          .transition()
          .duration(1100)
          .attr("opacity", 1),
        update => update,
        exit => exit.remove())
      this.selections.edge = this.edgeGroup.selectAll("line")
      this.selections.edge.attr("stroke-width", (d) => d.width)
      this.selections.edge.attr("stroke", (d) => d.colour)
  
      // setup circle dragging
  
      const node_drag = d3.drag()
        //the alpha for the simulation will cause the forces to act until it gets to zero
        .on("start", (event) => {
          this.simulation.alphaTarget(0.5).restart()
          //set node to its own x and y
          event.subject.fx = event.x
          event.subject.fy = event.y
        })
        .on("drag", (event) => {
          //pull the node on drag and keep it pinned to the pointer with fx/fy. 
          event.subject.fx = event.x
          event.subject.fy = event.y
        })
        .on("end", (event) => {
          this.simulation.alphaTarget(0)
          event.subject.fx = null
          event.subject.fy = null
        })
      this.selections.circle.call(node_drag)
  
      this.updatePositions()
    }
  
    updatePositions() {
      this.selections.circle
        .attr("cx", d => d.x)
        .attr("cy", d => d.y)
      this.selections.text
        .attr("x", d => d.x)
        .attr("y", d => d.y)
      this.selections.edge
        .attr("x1", d => d.source?.x)
        .attr("y1", d => d.source?.y)
        .attr("x2", d => d.target?.x)
        .attr("y2", d => d.target?.y)
    }

  }