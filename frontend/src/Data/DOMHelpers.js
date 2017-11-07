

function genValue(node) {
    console.log('gotnode', arguments)
  return function() {
    return node.value
  }
}

function setValue(text) {
  return function(node) {
    return function() {
      node.value = text
    }
  }
}

module.exports = {
  value: genValue,
  setValue: setValue
}
