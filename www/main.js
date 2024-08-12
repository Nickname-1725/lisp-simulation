
// Ajax 发送get请求, 封装
function Ajax_get (url, success, fail) {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url);
  xhr.send();
  xhr.onreadystatechange = function () {
    // 成功
    if (xhr.readyState == 4 && xhr.status == 200) {
      success && success(xhr.responseText); // 如果函数传入success, 则调用
    } else if (xhr.readyState == 4 && xhr.status == 404) {
      fail && fail (); // 如果函数传入fail, 则调用
    }
  }
}

// Ajax 发送post请求, 封装
function Ajax_post (url, obj, success, fail) {
  const xhr = new XMLHttpRequest();
  xhr.open ('POST', url, true);
  xhr.setRequestHeader("Content-type","application/json; charset=utf-8");

  xhr.send (JSON.stringify (obj));
  xhr.onreadystatechange = function () {
    // 成功
    if (xhr.readyState == 4 && xhr.status == 200) {
      success && success(xhr.responseText); // 如果函数传入success, 则调用
    } else if (xhr.readyState == 4 && xhr.status == 404) {
      fail && fail (); // 如果函数传入fail, 则调用
    }
  }
}

const display = document.getElementById('display');
const draw_kit = 
  {
    line: function (attr_obj, context) {
      context.beginPath();
      context.lineWidth = attr_obj['width'];
      context.moveTo(attr_obj['x1'], attr_obj['y1']);
      context.lineTo(attr_obj['x2'], attr_obj['y2']);
      context.stroke();
      return null;
    }
  }

function elem_parser (elem_obj, context, draw_kit) {
  "根据elem_obj，从draw_kit中选择函数来执行，在context上绘制图像"
  const elem_type = Object.keys(elem_obj)[0];
  const attr_obj = elem_obj [elem_type];
  
  draw_kit[elem_type](attr_obj, context);
  return null;
}

const render_list = [];
const canvas = document.getElementsByTagName('canvas')[0];

function make_anim_player (frame_list, context_decorator) {
  "生成一个播放器; group_decorator是给g调用的设定样式的函数"
  const len = frame_list.length;
  let index = 0;
  const context = canvas.getContext('2d');
  const player = 
    function () {
      let frame = frame_list[index];
      context_decorator && context_decorator(context);
      frame.map((elem_obj) => (elem_parser (elem_obj, context)));
      index = (index + 1) % len;
    }
  return player;
}

function request_ami (name, uri, config, create_player, force) {
  const post_request = 
    () =>
      (Ajax_post (uri + '&name=' + name, config, 
                  (text) => (create_player && create_player(JSON.parse(text)))))
  const get_request = 
    () => 
      (Ajax_get ('./resource/' + name + '.json', 
                 (text) => (create_player && create_player (JSON.parse(text))),
                 post_request // 如果请求失败, 改POST请求
                ))
  if (force) 
  {
    post_request()
  } // 强制更新
  else 
  {
    get_request()
  }
}

/*function request_draw_tree (name, frame, obj, color, force) {
  request_ami (name, "/swing-tree?frame=" + frame, obj,
               (data) => 
                 { 
                   let swing_tree_player = 
                   make_anim_player 
                   (data, 
                    (context) => 
                      {
                        context.lineCap = 'round';
                        context.strokeStyle = color;
                      });
                   render_list.push(swing_tree_player);
                 },
               force);
}*/

/*
// 更远景
request_draw_tree("swing-tree-31", 110, {trunk:{len: 95, wdth: 10, depth:6, x:350, y: 400}}, '#123491');
request_draw_tree("swing-tree-32", 105, {trunk:{len: 100, wdth: 10, depth:6, x:150, y: 400}}, '#123491');
request_draw_tree("swing-tree-33", 90, {trunk:{len: 110, wdth: 10, depth:6, x:550, y: 400}}, '#123491');
request_draw_tree("swing-tree-34", 95, {trunk:{len: 105, wdth: 10, depth:6, x:750, y: 400}}, '#123491');
request_draw_tree("swing-tree-35", 112, {trunk:{len: 90, wdth: 10, depth:6, x:950, y: 400}}, '#123491');
request_draw_tree("swing-tree-36", 100, {trunk:{len: 100, wdth: 10, depth:6, x:1150, y: 400}}, '#123491');

// 远景
request_draw_tree("swing-tree-21", 80, {trunk:{len: 200, wdth: 21, depth:8, x:100, y:500}}, '#6c371f');
request_draw_tree("swing-tree-22", 100, {trunk:{len: 200, wdth: 20, depth:8, x:520, y:500}}, '#6c371f');
request_draw_tree("swing-tree-23", 95, {trunk:{len: 200, wdth: 20, depth:8, x:880, y:500}}, '#6c371f');
request_draw_tree("swing-tree-24", 90, {trunk:{len: 200, wdth: 20, depth:8, x:1300, y:500}}, '#6c371f');

// 近景
request_draw_tree("swing-tree-11", 60, {trunk:{len: 350, wdth: 40, depth:9, x:1000, y: 600}}, '#b79477'); */

// 负责动画播放的函数
function refresh (timestamp) {
  if (timestamp) { // 或许timestamp也可以取消掉
    canvas.width = canvas.width; // 清空画布
    render_list.map((fun) => (fun()));
  }
  requestAnimationFrame(refresh);
}

// 启动入口
refresh();
