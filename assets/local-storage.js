var ElmLocalStoragePorts = function() {};

ElmLocalStoragePorts.prototype.subscribe =
  function(app, getPortName, setPortName, clearPortName, responsePortName, listKeysPortName) {

    if (!getPortName) getPortName = "getItem";
    if (!setPortName) setPortName = "setItem";
    if (!clearPortName) clearPortName = "clear";
    if (!listKeysPortName) listKeysPortName = "listKeys";
    if (!responsePortName) responsePortName = "response";

    var responsePort = app.ports[responsePortName];

    app.ports[getPortName].subscribe(function(key) {
      var val = null;
      try {
        val = JSON.parse(localStorage.getItem(key))
      } catch (e) {}
      responsePort.send({
        key: key,
        value: val
      })
    });

    app.ports[setPortName].subscribe(function(kv) {
      var key = kv[0];
      var json = kv[1];
      if (json === null) {
        localStorage.removeItem(key);
      } else {
        localStorage.setItem(key, JSON.stringify(json));
      }
    });

    app.ports[clearPortName].subscribe(function(prefix) {
      if (prefix) {
        var cnt = localStorage.length;
        for (var i = cnt - 1; i >= 0; --i) {
          var key = localStorage.key(i);
          if (key && key.startsWith(prefix)) {
            localStorage.removeItem(key);
          }
        }
      } else {
        localStorage.clear();
      }
    });

    app.ports[listKeysPortName].subscribe(function(prefix) {
      var cnt = localStorage.length;
      var keys = [];
      for (var i = 0; i < cnt; i++) {
        var key = localStorage.key(i);
        if (key && key.startsWith(prefix)) {
          keys.push(key);
        }
      }
      responsePort.send({
        prefix: prefix,
        keys: keys
      });
    });
  };

module.exports.ElmLocalStoragePorts = ElmLocalStoragePorts;
