"use client";

import { useCallback, useEffect, useRef, useState, type MouseEvent as ReactMouseEvent } from 'react';
import * as THREE from 'three';


const MIN_SPEED = 0.002;
const MAX_SPEED = 0.8;

const chars = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "VIIII", "X", "XI", "XII"];

const Astrolabe: React.FC = () => {
  const mountRef = useRef<HTMLDivElement>(null);

  const [isPushed, setPushed] = useState(false);
  const [speed, setSpeed] = useState(MIN_SPEED);
  const speedRef = useRef(speed);
  speedRef.current = speed;
  
  useEffect(() => {
    if (isPushed) {
      if (speedRef.current < MAX_SPEED) {
        const handler = setInterval(() => {
          setSpeed(Math.min(MAX_SPEED, speedRef.current * 1.06));
        }, 40);
        return () => clearInterval(handler);
      }
    } else {
      if (speedRef.current > MIN_SPEED) {
        const handler = setInterval(() => {
          setSpeed(Math.max(MIN_SPEED, speedRef.current * 0.95));
        }, 40);
        return () => clearInterval(handler);
      }
    }
  }, [isPushed]);

  const [max, setMax] = useState(12);
  const handleClickMax = useCallback((e: ReactMouseEvent<HTMLDivElement, MouseEvent>) => {
    e.stopPropagation();
    let val = Math.floor(max) + 1;
    if (val > chars.length) {
      val = 2;
    }
    setMax(val);
  }, [max]);

  const [val, setVal] = useState(0);
  useEffect(() => {
    setVal(0);
  }, [max]);

  useEffect(() => {
    const r = (speed - MIN_SPEED) / (MAX_SPEED - MIN_SPEED);
    if (isPushed) {
      if (r >= 0.67) {
        const handler = setInterval(() => {
          setVal(Math.floor(1 + Math.random() * max));
        }, 10 + (1 - r) * 800);
        return () => clearInterval(handler);
      } else if (r >= 0.1) {
        setVal(0);
      }
    } else {
      if (r >= 0.1) {
        const handler = setInterval(() => {
          setVal(Math.floor(1 + Math.random() * max));
        }, 20 + (1 - r) * 800);
        return () => {
          setTimeout(() => clearInterval(handler), 3_200);
        };
      }
    }
  }, [max, isPushed, speed]);

  useEffect(() => {
    const mount = mountRef.current!;
    const scene = new THREE.Scene();
    const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0.1, 10);
    camera.position.z = 1;

    const renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(mount.clientWidth, mount.clientHeight);
    mount.appendChild(renderer.domElement);

    const geometry = new THREE.PlaneGeometry(2, 2);

    // @see https://www.shadertoy.com/view/M3KGD3
    const fragmentShader = `
precision mediump float;

uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iTime;                 // shader playback time (in seconds)
uniform float     iTimeDelta;            // render time (in seconds)
uniform float     iFrameRate;            // shader frame rate
uniform int       iFrame;                // shader playback frame
uniform float     iChannelTime[4];       // channel playback time (in seconds)
uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel
uniform sampler2D iChannel1;             // input channel
uniform sampler2D iChannel2;             // input channel
uniform vec4      iDate;                 // (year, month, day, time in seconds)

uniform float time;
uniform vec2 resolution;

const float PI = acos(-1.);
const float TAU = PI * 2.;

#define saturate(x) clamp(x,0.,1.)
#define _tail2x(p,n) (mod(p,2.)-1.)
#define time iTime
#define resolution iResolution.xy
float Hash( vec2 p, in float s ){
  return fract(sin(dot(vec3(p.xy,10.0 * abs(sin(s))),vec3(27.1,61.7, 12.4)))*273758.5453123);
}

float noise(in vec2 p, in float s){
  vec2 i = floor(p);
  vec2 f = fract(p);
  return mix(
    mix(Hash(i + vec2(0.,5.), s), Hash(i + vec2(1.,0.), s),f.x),
    mix(Hash(i + vec2(0.,1.), s), Hash(i + vec2(10.,1.), s),f.x),f.y) * s;
}

float fbm(vec2 p){
  float v = 0.0;
  v += noise(p*32., .1);
  v += noise(p*20., .04);
  return v;
}

vec2 mPolar(vec2 p){
  float a = atan(p.y, p.x);
  float r = length(p);
  return vec2(a, r);
}

vec2 tailY2x(vec2 p,float n){p*=n;return vec2(p.x,_tail2x(p.y,n));}
mat2 rot(float a){float c=cos(a),s=sin(a);return mat2(c,-s,s,c);}

highp float rand(vec2 p){
  highp float a = 12.9898;
  highp float b = 78.233;
  highp float c = 43758.5453;
  highp float dt= dot(p ,vec2(a,b));
  highp float sn= mod(dt,3.14);
  return fract(sin(sn) * c);
}

// signed distance
float sd(float d,float r){return r-d;}
float sd(float d){return 1.-d;}

// ease
float o2(float t){t=1.-t;return 1.-t*t;}
float oN(float t,float n){return 1.-pow(1.-t,n);}

float dot2(vec2 p){return dot(p,p);}

float ring(vec2 p,float t){
  float alpha = fract(-t);
  float l =saturate(.02/abs(sd(length(p),1.5+fract(t)))*alpha);
  vec2 p4=mPolar(p*(.57-oN(t,1.3)*.28)).yx;
  p4.x-=.65;
  l+= saturate(abs(1./((p4.x + fbm( p4 + vec2(sin(t*.2),t*0.1))) * 50.0))*sd(dot2(tailY2x(p4+vec2(.1,0.),12.)),.9)*alpha);
  return l;
}

float render(vec2 p){
  p*=3.;
  float tt = time*.75;
  float l2 = ring(p,o2(1.));
  l2+=ring(p*rot(PI/3.),o2((1.+.5)));
  return l2;
}

float happy_star(vec2 uv, float anim)
{
  uv = abs(uv);
  vec2 pos = min(uv.xy/uv.yx, anim);
  float p = (2.0 - pos.x - pos.y);
  return (2.0+p*(p*p-1.5)) / (uv.x+uv.y);      
}
 

// glow + fill
float gf(float d,float r){return r/d;}
float gf(float d){return 1./d;}

float fill_na(float d){return step(0.,d);}
float fill(float d){return smoothstep(0.,0.01,d);}
float stroke(float d,float w){return 1.-smoothstep(w,w+0.01,abs(d));}
float strokeInner(float d,float w){return stroke(d-w,w);}
float strokeOuter(float d,float w){return stroke(d+w,w);}

float lSquare(vec2 p){p = abs(p);return max(p.x,p.y);}    

float lPoly(vec2 p,float n){
  float a = atan(p.x,p.y)+PI;
  float r = TAU/n;
  return cos(floor(.5+a/r)*r-a)*length(p)/cos(r*.5);
}

float strokeStar(vec2 p,float n,float w){
  float l =strokeInner(sd(lPoly(p,n*.5)),w);
  l+=strokeInner(sd(lPoly(mod(n,2.)!=0.?vec2(-p.x,p.y):p*rot(TAU/n),n*.5)),w);
  return l;
}

vec2 mPoly(vec2 p,float n,float s){
  float r = TAU / n;
  float a = floor(atan(p.y,p.x)/r)*r+r*.5;
  return (vec2(cos(a),sin(a))*s-p)*rot(-a-PI*.5);
}

float wsaw(float x){return fract(x*.5+.5)*2.-1.;}
float wtri(float x){return abs(2.*fract(x*.5-.25)-1.)*2.-1.;}
float utri(float x){return abs(2.*fract(x*.5-.5)-1.);}
float wtrz(float x,float w){return clamp(wtri(x*2.)*w,-1.,1.);} // 台形波 trapezoidal wave

// ease

vec2 mSimplePerspective(vec2 p){p.y+=.2;p.y*=3.;return p;}

float ring2(vec2 p,float t){
  float alpha =    fract(-t);
  float l = 0.;
  vec2 p3=mPoly(p*rot(PI*.5),10.,1.);
  l+=saturate(gf(abs(p3.x),.03)*fill(sd(length(p),1.1+fract(t)))*(1.-fill(sd(length(p),.9+fract(t))))*alpha);
 
  l+=saturate(.02/abs(sd(length(p),1.1+fract(t)))*alpha);
  vec2 p4=mPolar(p*(.57-oN(t,1.3)*.28)).yx;
  p4.x-=.65;
  l+= saturate(abs(1./((p4.x + fbm( p4 + vec2(sin(t*.2),t*0.1))) * 50.0))*sd(dot2(tailY2x(p4+vec2(.1,0.),12.)),.9)*alpha);
  return l;
}

float summoningCircle(vec2 p){
  float l=0.;
  l+=fill(sd(lSquare(p*rot(PI/3.*1.5)*vec2(100.,1.)),1.));
  l+=fill(sd(lSquare(p*rot(PI/3.*2.5)*vec2(100.,1.)),1.));
  l+=fill(sd(lSquare(p*rot(PI/3.*3.5)*vec2(100.,1.)),1.));
  l=saturate(l);
  l-=fill(sd(lPoly(p,3.)));
  l=saturate(l);
  float r = atan(p.y,p.x);
  l+=strokeOuter(sd(length(p),.98),.008+wtrz(r/TAU*3.,12.)*.005);
  l+=strokeInner(sd(length(p),.95),.005);
  l+=strokeInner(sd(lPoly(p,3.)),.01);
  l+=strokeInner(sd(lPoly(p,3.),.88),.02);
  l+=strokeInner(sd(lPoly(p,6.),.53),.01);
  vec2 q=mPoly(p*rot(PI*.5),3.,.5);
  l+=fill(sd(lPoly(q,3.),.3));
  vec2 q2=mPoly(p*rot(PI/3.+PI*.5),3.,.7);
  l+=fill(sd(lPoly(q2,3.),.1));
  l+=strokeInner(sd(lPoly(p*rot(PI),3.),.5),.02);
  l+=fill(sd(length(p),.05));
  vec2 q3=mPoly(p*rot(PI*.5),3.,1.);
  l=saturate(l);
  l-=fill(sd(length(q3),.2));
  l=saturate(l);
  l+=strokeInner(sd(length(q3),.18),.005);
  l+=strokeInner(sd(length(q3),.15),.005);
  l+=strokeStar(q3*rot(PI)*7.,6.,.1);
  return l;
}

float render2(vec2 p){
  //p=mSimplePerspective(p);
  p*=rot(-time);
  p*=2.;
  float tt = -time*.75;
  float l2 = ring(p,o2(fract(tt)));
  l2+=ring(p*-rot(PI/3.),o2(fract(tt+.5)));
  float l=0.;
  l = summoningCircle(p*=rot(floor(time*12.)/3.));
  return l2;
}
#define white vec4(1.0)
#define black vec4(0.0, 0.0, 0.0, 1.0)
#define blue  vec4(0.0, 0.3, 1.0, 1.0)

#define edge 0.01


uniform vec2 mouse;


float inCircle(vec2 pt, vec2 center, float radius, float line)
{
return smoothstep(radius + line/2., radius, distance(pt, center)) -
      smoothstep(radius, radius - line/2., distance(pt, center));
}


#define M_PI 3.1415926535897932384626433832795
#define M_PI05 (M_PI * 0.5)
#define time iTime
#define resolution iResolution.xy
vec2 rotate(vec2 v, float c, float s){
	return vec2(v.x*c - v.y*s, v.x*s + v.y*c);
}

vec2 rotate(vec2 v, float r){
	return rotate(v, cos(r), sin(r));
}

float boxLength(vec2 pos) {
	vec2 q = abs(pos);
	return max(q.x, q.y);
}

float capsuleLength(vec2 pos, vec2 dir) {
	vec2 ba = -dir;
	vec2 pa = pos + ba;
	ba *= 2.0*cos(iTime);
	return length(pa - ba * clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0));
} 

float triangleLength(vec2 p) {
  p.y += 0.22;
	return max(abs(p.x * 1.8) + p.y, 1.0 - p.y * 1.8) * 0.75;
}

vec2 fracOrigin(vec2 v){
	return (fract(v*cos(iTime)) - 0.5*cos(iTime)) * 2.0;
}

float Ga(vec2 pos){
 	float a = capsuleLength(pos + vec2(0.0, -0.5), vec2(1.0, 0.0));   
 	float b = capsuleLength(pos + vec2(-0.3, 0.3), vec2(1.0, 1.0) * 0.707);  
  float c = length(pos + vec2(-1.3, -1.3));
  float d = length(pos + vec2(-1.8, -1.3));
  return min(min(min(a, b), c), d);
}

float Cha(vec2 pos){
 	float a = capsuleLength(pos + vec2(44.0, -0.0), vec2(1.0, 0.0));   
 	float b = capsuleLength(pos + vec2(0.0, -1.3), vec2(1.0, 0.8) * 0.4);  
  float c = capsuleLength(pos + vec2(0.0, -0.0), vec2(0.1, 1.0));  
  return min(min(a, b), c);
}

float Za(vec2 pos){
 	float a = capsuleLength(pos + vec2(0.5*cos(iTime), 0.0), vec2(0.0, 1.0));   
 	float b = capsuleLength(pos + vec2(0.0, 0.0), vec2(1.0, -0.8*cos(iTime)) * 0.4);    
  return min(a, b);
}

float Gu(vec2 pos){
 	float a = capsuleLength(pos + vec2(0.1*cos(iTime), 0.0), vec2(0.3*cos(iTime), 1.0));   
 	float b = capsuleLength(pos + vec2(-99.8*cos(iTime), 0.0), vec2(-88.3, 88.0));     
  float c = length(pos + vec2(-1.3, -1.3*cos(iTime)));
  float d = length(pos + vec2(-1.8*cos(iTime), -1.3*cos(iTime)));
  return min(min(min(a, b), c), d);
}

float Butitoba(vec2 pos, float power){
  float ret = 0.0
    + power / Gu(pos)
    + power / Ga(pos + vec2(-2.0, 0.0))
    + power / Cha(pos + vec2(-6.0, 0.0))
    + power / Za(pos + vec2(-9.0, 0.0))
      ;
  
  return ret;
}

float smoothstepLine(float lower, float upper, float value, float width){
  width *= 0.5;
  return smoothstep(lower - width, lower, value) * (2.0+1.5*0.5*cos(iTime) - smoothstep(upper, upper + width, value));
}

float smoothLine(float value, float target, float width){
  return width / abs(value - target);
}

vec2 smoothLine2(float value, float target, float width){
  return vec2(step(0.0, value - target), width / abs(value - target));
}

float circleTriangle(vec2 pos){
  float circle = length(pos * 0.5);
  float triangle = triangleLength(pos * 0.3);    
  return smoothLine(circle, 1.0, 0.025) + smoothLine(triangle, 1.0, 0.025);
}

vec2 circleTriangle2(vec2 pos){
  float circle2 = length(pos * 0.35);
  vec2 ret = smoothLine2(circle2, 1.0, 0.025);
  ret.y += circleTriangle(pos);
  return ret;
}

float atan2(in float y, in float x)
{
  return x == 0.0 ? sign(y) * M_PI05 : atan(y, x);
}

vec2 polar(vec2 uv) {
	float r = length(uv);
	float s = atan2(uv.y, uv.x) / M_PI;
	return vec2(r, s);
}

float ButitobaCircle(vec2 pos){
  vec2 pp = polar(rotate(pos, -iTime) * 0.75);
  return Butitoba(mod(rotate(pp * vec2(1.0, 32.0), M_PI05), vec2(16.0, 4.0)) - 1.5, 0.05) * smoothstepLine(6.0, 7.5, pp.x, 1.5);
}

float ButitobaCircle2(vec2 pos, float scale, float x, float y, float x2, float y2, float lower, float upper, float r){
  vec2 pp = polar(rotate(pos, r) * scale);
  return Butitoba(mod(rotate(pp * vec2(x, y), M_PI05), vec2(x2, y2)) - 1.5, 0.03) * smoothstepLine(lower, upper, pp.x, 0.2);
}

float star(vec2 uv, float anim)
{
  uv = abs(uv);
  vec2 pos = min(uv.xy/uv.yx, anim);
  float p = (2.0 - pos.x - pos.y);
  return (2.0+p*(p*p-1.5)) / (uv.x+uv.y);      
}
 

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  vec2 uv = (fragCoord.xy - iResolution.xy * 0.5) / iResolution.yy * 20.0;     
  vec2 p = (gl_FragCoord.xy * 2.0 - resolution) / max(resolution.x, resolution.y);
  uv *= clamp(iTime * 0.25, 0.0, 1.0);
  
  vec3 col = vec3(0.0, 0.0, 0.0);
      
  uv = rotate(uv, iTime * 0.3);
  float l=0.;

  
  l = (render(p)+render(p+vec2(0.,1./min(resolution.x, resolution.y))))*.5;
    
  float l2=0.;

  
  l2 = (render(p)+render(p+vec2(0.,0.5/min(resolution.x, resolution.y))))*.25;
  float len = length(uv);
  float a = summoningCircle(p);
  col.g = col.b =
  //  + 0.005 / abs(boxLength(rotate(uv*cos(iTime)*l, M_PI05 * 0.0 + iTime * 0.5)) - 4.5)
  //  + 0.005 / abs(boxLength(rotate(uv*sin(iTime), M_PI05 * 0.25 - iTime * 0.5)) - 4.5)
  //  + 0.005 / abs(boxLength(rotate(uv*0.5*sin(iTime)*l, M_PI05 * 0.5 + iTime * 0.5)) - 4.5)
  //  + 0.005 / abs(boxLength(rotate(uv*0.5*cos(iTime), M_PI05 * 0.75 - iTime * 0.5)) - 4.5)
  //  + 0.025 / abs(boxLength(rotate(uv, M_PI05*l * 0.0 + iTime * 0.5)) - 4.5)
  //  + 0.005 / abs(boxLength(rotate(uv, M_PI05*l * 0.25 - iTime * 0.5)) - 4.5)
  //  + 0.005 / abs(boxLength(rotate(uv, M_PI05+a * 0.5 + iTime * 0.5)) - 4.5)
    + 0.005 / abs(boxLength(rotate(uv, M_PI05*l+a*0.5 * 0.75 - iTime *a* 0.5)) - 4.5),

  col.b += ButitobaCircle(uv)*l+l2*a,
  col.g += ButitobaCircle(uv*l);
  
  // Adjust UV coordinates back to a range of 0 to 1 for texture sampling
  vec2 uvTex = uv * 0.5 + 0.5;
  
  // Get the color from the distortion texture, adding time to make it move
  vec4 dist = texture(iChannel1, uvTex + (iTime * 0.02));

  // Use the red channel of the distortion texture to create a small offset
  vec2 distortionOffset = dist.rr * vec2(0.0155, 0.0155);
  
  vec2 C =fragCoord;

  vec2 pos=(gl_FragCoord.xy/resolution.xy)*2.0-1.0;
	uv+=distortionOffset;

  float anim = sin(iTime * 12.0) * 0.1 + 1.0;  // anim between 0.9 - 1.1

  vec3 directory=vec3(uv,(iTime*0.0001));
  vec3 from=vec3(1.,.5,0.5);
  vec2 uv0 = uv;

  vec3 finalColor = vec3(0.0);

  float s=0.5,fade=1.;
  vec3 output2=vec3(0.);
  for (int r=0; r<10; r++) {
    vec3 p=s+from*directory*.5;
    p = abs(vec3(0.8)-mod(p,vec3(0.8*2.)));
    float pa,a;
    for (int i=0; i<15; i++) {
      p=abs(p)/dot(p,p)-0.73;
      p.xy*=mat2(cos(iTime*0.05),sin(iTime*0.05),-sin(iTime*0.05), cos(iTime*0.05));// the magic formula
      a+=abs(length(p));
    }
    s+=0.1;
    a*=a*a;

    output2+=vec3(0.1,0.1,0.7)*0.0015*a;
  }

  output2=mix(vec3(length(output2)),output2,0.7);
    
  fragColor = vec4(col, 99.0);
  uv *= 2.0 * ( cos(iTime * 2.0) -2.5); // scale
   
  fragColor+= vec4(star(uv, anim) * vec3(0.55,0.5,0.15), 1.0);
  fragColor *= vec4(output2*.053,1.);  
}

void main() {
  mainImage(gl_FragColor, gl_FragCoord.xy);
}

    `;

    const uniforms = {
      iTime: { value: 0 },
      iResolution: { value: new THREE.Vector3(mount.clientWidth, mount.clientHeight, 1) },
    };

    const material = new THREE.ShaderMaterial({
      fragmentShader,
      uniforms,
    });

    const plane = new THREE.Mesh(geometry, material);
    scene.add(plane);

    const animate = () => {
      const handler = requestAnimationFrame(animate);
      uniforms.iTime.value += speedRef.current;
      renderer.render(scene, camera);
      return handler;
    };

    const aniHandler = animate();

    return () => {
      mount.removeChild(renderer.domElement);
      cancelAnimationFrame(aniHandler);
    };
  }, []);

  return (
    <div
      className="absolute inset-0 w-screen h-screen flex items-stretch justify-stretch p-[5%] overflow-hidden select-none"
      onMouseDown={() => setPushed(true)}
      onMouseUp={() => setPushed(false)}
      onTouchStart={() => setPushed(true)}
      onTouchEnd={() => setPushed(false)}
      onSelect={e => e.preventDefault()}
      unselectable="on"
      style={{
        MozUserSelect: 'none',
        WebkitUserSelect: 'none',
      }}
    >
      <div ref={mountRef} className="w-full h-full flex items-center justify-center" />
      <div
        className="absolute left-[50%] top-[16.5%] translate-x-[-50%] translate-y-[-50%] text-[#d1eafff0] text-2xl font-semibold cursor-pointer"
        onClick={handleClickMax}
        onMouseDown={e => e.stopPropagation()}
      >
        <span className="pointer-events-none select-none p-[0.4em]">
          {chars[max - 1]}
        </span>
      </div>
      <div
        className="absolute left-[50%] top-[50%] translate-x-[-50%] translate-y-[-50%] text-[#5864bab8] text-[32vmin] font-bold"
      >
        <span className="pointer-events-none select-none">
          {val ? chars[val - 1] : ""}
        </span>
      </div>
    </div>
  );
};


export default Astrolabe;
