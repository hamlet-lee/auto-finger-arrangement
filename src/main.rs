//use std::collections::HashMap;
use std::collections::HashMap;
use std::iter;
// https://docs.rs/internationalization/0.0.3/internationalization/
// use internationalization::t;
use std::fmt;
//use std::io::Read;

#[cfg(test)]
mod tests {
    use crate::{FingerStatus, create_finger_status_list_playing_note, ViolinString, Note, Finger, is_playing_note, StringAndPose, MyMap, Pose};

    #[test]
    fn t0 () {
        let note = Note {
            finger: Some(Finger::First),
            violin_string: ViolinString::G
        };
        let fs = FingerStatus {
          finger_status_map: MyMap {
              string_and_pose_arr: [
                  StringAndPose {
                      violin_string: ViolinString::G,
                      pose: Pose::DOWN
                  },
                  StringAndPose {
                      violin_string: ViolinString::G,
                      pose: Pose::LIFT
                  },
                  StringAndPose {
                      violin_string: ViolinString::G,
                      pose: Pose::LIFT
                  },
                  StringAndPose {
                      violin_string: ViolinString::G,
                      pose: Pose::LIFT
                  }
              ]}
        };
        assert!( is_playing_note(&note, &fs))
    }

    #[test]
    fn t1 () {
        let note = Note {
            finger: Some(Finger::First),
            violin_string: ViolinString::G
        };
        let vec_finger_status: Vec<FingerStatus>
            = create_finger_status_list_playing_note(
            &note, false);
        assert!( vec_finger_status.len() > 0)
    }
}
#[derive(Debug, Clone)]
struct MyMap {
    string_and_pose_arr: [StringAndPose;4]
}

impl MyMap {
    fn values(&self) -> impl Iterator<Item = (Finger, StringAndPose)> {
        let a = iter::once((Finger::First, self.string_and_pose_arr[0]));
        let b = iter::once((Finger::Second, self.string_and_pose_arr[1]));
        let c = iter::once((Finger::Third, self.string_and_pose_arr[2]));
        let d = iter::once((Finger::Fourth, self.string_and_pose_arr[3]));
        a.chain(b).chain(c).chain(d)
    }
    fn set(&mut self, idx: usize, node: StringAndPose) {
        self.string_and_pose_arr[idx] = node;
    }
    fn get(&self, idx: usize) -> StringAndPose{
        self.string_and_pose_arr[idx].clone()
    }
    fn insert(&mut self, f: Finger, v: StringAndPose) {
        self.set(match f {
            Finger::First => 0,
            Finger::Second => 1,
            Finger::Third => 2,
            Finger::Fourth => 3
        }, v)
    }
    fn get_by_finger (&self, f: &Finger) -> StringAndPose {
        self.get(match *f {
            Finger::First => 0,
            Finger::Second => 1,
            Finger::Third => 2,
            Finger::Fourth => 3
        })
    }
    fn new() -> MyMap {
        MyMap {
            string_and_pose_arr: [StringAndPose{
                violin_string: ViolinString::G,
                pose: Pose::LIFT
            };4]
        }
    }
}

// https://stackoverflow.com/questions/30218886/how-to-implement-iterator-and-intoiterator-for-a-simple-struct
impl IntoIterator for MyMap {
    type Item = (Finger, StringAndPose);
    type IntoIter = MyMapIntoIterator;

    fn into_iter(self) -> Self::IntoIter {
        MyMapIntoIterator {
            my_map: self,
            index: 0,
        }
    }
}

struct MyMapIntoIterator {
    my_map: MyMap,
    index: usize
}


impl Iterator for MyMapIntoIterator {
    type Item = (Finger, StringAndPose);
    fn next(&mut self) -> Option<(Finger, StringAndPose)> {
        let result = match self.index {
            0 => (Finger::First, self.my_map.string_and_pose_arr[0]),
            1 => (Finger::Second, self.my_map.string_and_pose_arr[1]),
            2 => (Finger::Third, self.my_map.string_and_pose_arr[2]),
            3 => (Finger::Fourth, self.my_map.string_and_pose_arr[3]),
            _ => return None,
        };
        self.index += 1;
        Some(result)
    }
}

#[derive(Debug, Clone)]
struct FingerStatus {
    finger_status_map: MyMap
}
//#[derive(Debug, Clone)]
//struct FingerStringAndPose {
//    finger: Finger,
//    violin_string: ViolinString,
//    action: Pose
//}
#[derive(Debug, Clone, PartialEq, Copy)]
struct StringAndPose {
    violin_string: ViolinString,
    pose: Pose
}
#[derive(Debug, Clone, PartialOrd, PartialEq, Copy)]
enum Pose {
    LIFT,
    DOWN,
    APPROACHING
}
impl fmt::Display for Pose{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Pose::APPROACHING => "下落中",
            Pose::DOWN => "按",
            Pose::LIFT => "抬"
        })
    }
}

#[derive(Debug, Clone)]
struct StageNodes {
    nodes: Vec<FingerStatusAndChoice>,
}

#[derive(Debug, Clone)]
struct FingerStatusAndChoice {
    finger_status: FingerStatus,
    choice: Choice
}

fn finger_status_to_str(fs: &FingerStatus) -> String {
    let mut v: Vec<String>  = Vec::new();
    for (idx, e) in fs.finger_status_map.string_and_pose_arr
        .iter().enumerate() {
            v.push(format!("[{}指 {} {}]", idx + 1, e.violin_string, e.pose))
        };
    v.join(" ")
}

#[derive(Debug, Clone)]
struct Choice {
    prev_pos: i32,
    accumulate_cost: f32
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
enum Finger {
    First,
    Second,
    Third,
    Fourth
}
impl fmt::Display for Finger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}指", match self {
            Finger::First => 1,
            Finger::Second => 2,
            Finger::Third => 3,
            Finger::Fourth => 4
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ViolinString {
    G, D, A, E
}
impl fmt::Display for ViolinString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}弦", match self {
            ViolinString::G => "G",
            ViolinString::D => "D",
            ViolinString::A => "A",
            ViolinString::E => "E"
        })
    }
}

#[derive(Debug, Clone)]
struct Note {
    finger: Option<Finger>,
    violin_string: ViolinString
}

impl fmt::Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.violin_string, match &self.finger {
            Some(fg) => format!("{}", fg),
            None => String::from("0")
        })
    }  
}

fn enumerate_violin_string_and_action(allow_approaching: bool) -> Vec<(ViolinString, Pose)> {
    let mut ret_vec = Vec::new();
    for violin_string in &[ViolinString::G, ViolinString::D, ViolinString::A, ViolinString::E] {
        let pose_list = if allow_approaching {
            vec!(Pose::DOWN, Pose::LIFT, Pose::APPROACHING)
        } else {
            vec!(Pose::DOWN, Pose::LIFT)
        };
        for action in pose_list {
            ret_vec.push( (violin_string.clone(), action.clone()) );
        }
    }
    ret_vec
}


fn enumerate_string_action_for_four(allow_approach: bool) -> Vec<Vec<(ViolinString,Pose)>> {
    let mut v : Vec<(ViolinString, Pose)> = Vec::new();
    for _i in 0..4 {
        v.push((ViolinString::G, Pose::LIFT));
    }

    let mut ret : Vec<Vec<(ViolinString,Pose)>> = Vec::new();
    fill(&mut v, 0, & mut ret, allow_approach);
    ret
}

fn fill(mut v: & mut Vec<(ViolinString, Pose)>,
        idx: i32,
        out: & mut Vec<Vec<(ViolinString, Pose)>>,
        allow_approach: bool) {

    if idx >= 4 {
        out.push(v.clone());
        return;
    } else {
        let enum_of_string_action : Vec<(ViolinString, Pose)> = enumerate_violin_string_and_action(allow_approach);
        for string_action in enum_of_string_action {
            v[idx as usize] = (string_action.0, string_action.1);
            fill(&mut v, idx + 1, out, allow_approach);
        }
    }
}

fn down_finger_no_disturb(finger: Finger, playing: Option<Finger>) -> bool {
    match playing {
        None => false,
        Some(pf) => finger <= pf
    }
}

fn create_finger_status_list_playing_note(
    playing_note: &Note,
    allow_approaching: bool) -> Vec<FingerStatus> {

    let possibles = enumerate_string_action_for_four(allow_approaching);
    let vfap : Vec<FingerStatus> = possibles.into_iter().map(|x| {
        let mut v : MyMap = MyMap::new();
        let flist = &[Finger::First, Finger::Second, Finger::Third, Finger::Fourth];
        for i in 0..4 {
            let sp = StringAndPose{
                violin_string: x[i].0.clone(),
                pose: x[i].1.clone()
            };
            v.insert(flist[i].clone(), sp);
        }
        FingerStatus {
            finger_status_map : v
        }
    }).collect();

    // deal with playing note
    let r = vfap.into_iter().filter( |fs| {
        let t1 = is_playing_note(playing_note, fs);
        // println!("is_playing_note({:?}, {:?}) : {}", 
        //     playing_note, fs, t1);
        t1
    }).collect();
    return r;
//    vec!(FingerStatus {finger_status_list: vec!(FingerStringAndPose{
//        finger: Finger::First,
//        violin_string: ViolinString::G,
//        action: Pose::LIFT
//    })})
}

fn is_playing_note(playing_note: &Note, fs: &FingerStatus) -> bool {
    fs.finger_status_map.values().any(|fsp| {
        match &playing_note.finger {
            None => true,
            Some(pf) => *pf == (fsp.0) && fsp.1.pose == Pose::DOWN
                && fsp.1.violin_string == playing_note.violin_string
        }
    })
    && fs.finger_status_map.values().all(|fsp| {
        match fsp.1.pose {
            Pose::LIFT | Pose::APPROACHING => true,
            Pose::DOWN => {
                if fsp.1.violin_string != playing_note.violin_string {
                    true
                } else {
                    down_finger_no_disturb(fsp.0.clone(), playing_note.finger.clone())
                }
            }
        }
    })
}
const STRING_SWITCH_COST : f32 = 1.0;
const HARD_COST: f32 = 1.0;
const IMPOSSIBLE_COST: f32 = 10000.0;
const NATUAL_COST: f32 = 0.1;
const NO_COST: f32 = 0.0;
fn transition_cost(from: &FingerStatus, to: &FingerStatus) -> f32 {
    let mut cost: f32 = 0.0;
    for i in &[Finger::First, Finger::Second, Finger::Third, Finger::Fourth] {
        let from_sp = from.finger_status_map.get_by_finger(i);
        let to_sp = to.finger_status_map.get_by_finger(i);
        if from_sp.violin_string != to_sp.violin_string { cost += STRING_SWITCH_COST; }
        cost += match (from_sp.pose, to_sp.pose ) {
            (Pose::APPROACHING, Pose::DOWN) => NATUAL_COST,
            (Pose::APPROACHING, _) => IMPOSSIBLE_COST, // else not allowed
            (Pose::DOWN, Pose::LIFT) => NATUAL_COST,
            (Pose::LIFT, Pose::APPROACHING) => NATUAL_COST,
            (_, Pose::APPROACHING) => IMPOSSIBLE_COST, // else not allowed
            (Pose::LIFT, Pose::DOWN) => HARD_COST,
            (Pose::LIFT, Pose::LIFT) | (Pose::DOWN, Pose::DOWN) => NO_COST
        }
    };
    cost

}
fn find_best_way (from:&StageNodes, to: & mut StageNodes) -> () {
    println!("finding from {} states to {} states", from.nodes.len(), to.nodes.len());
    for i in 0..to.nodes.len() {
        if i % 100 == 0 {
            println!("searched {} in {}", i, to.nodes.len());
        }
        let mut min_cost: f32 = 10000.0;
        let mut min_cost_idx: i32 = -1;
        for j in 0..from.nodes.len() {
            if from.nodes[j].choice.accumulate_cost >= min_cost ||
                from.nodes[j].choice.accumulate_cost >= IMPOSSIBLE_COST {
                continue;
            }
            let cost = from.nodes[j].choice.accumulate_cost + transition_cost(&from.nodes[j].finger_status, &to.nodes[i].finger_status);
            if cost < min_cost as f32 {
                min_cost = cost;
                min_cost_idx = j as i32;
            }
        }
        to.nodes[i].choice.prev_pos = min_cost_idx;
        to.nodes[i].choice.accumulate_cost = min_cost;
    }
    
    let mut m: HashMap<i32, u32> = HashMap::new();
    // https://users.rust-lang.org/t/efficient-string-hashmaps-for-a-frequency-count/7752
    for n in to.nodes.iter() {
        let cost = n.choice.accumulate_cost;
//        let prev = m.get(&(cost as i32));
//        match prev {
//            Some(v) => m.insert(cost as i32, v + 1),
//            None => m.insert(cost as i32, 1)
//        };
        *m.entry(cost as i32).or_insert(0) += 1;
    }
    println!("stage accumulate cost dist.: {:?}", m);
}

fn compute_transition(from: &FingerStatus, to: &FingerStatus) -> String {
    // "xxx".to_string()
    let mut moves : Vec<String> = Vec::new();
    for f in &[Finger::First, Finger::Second, Finger::Third, Finger::Fourth] {
        let sf_from = &from.finger_status_map.get_by_finger(f);
        let sf_to = &to.finger_status_map.get_by_finger(f);
        if sf_from != sf_to {
            moves.push( format!("Finger {:?} from {:?} to {:?}",
                                f, sf_from, sf_to))
        }
    }
    moves.into_iter().collect()
}
fn main() {
    // let score = vec!(
    //     Note{ finger:None, violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::Fourth), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
    //     Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
    //     Note{ finger:None, violin_string: ViolinString::G},
    // );

    let score = vec!(
        Note{ finger:Some(Finger::Fourth), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
    );
    compute(&score);
}

fn score_to_str(score: &Vec<Note>) -> String {
    score.iter().map(|x| format!("{}",x))
        .collect::<Vec<String>>()
        .join(" | ")
}


fn compute (score:&Vec<Note>) {
    let lang = "cn";
    let mut stages:Vec<StageNodes> = Vec::new();
    let mut pos = 0;
    println!("为演奏如下乐谱做规划: {:?}", 
        score_to_str(score));
    for note in score.clone() {
        println!("creating state list for note {:?} ...", note);
        let vec_finger_status: Vec<FingerStatus>
            = create_finger_status_list_playing_note(
            &note, false);
        let nodes: Vec<_> = vec_finger_status.iter().map( | f |
            FingerStatusAndChoice {
                finger_status: f.clone(),
                choice: Choice {prev_pos: -1, accumulate_cost: 0.0 }
        }).collect();
        println!("created state list with length: {}", nodes.len());
        stages.push(StageNodes{nodes});
        if pos > 0 {
            // https://stackoverflow.com/questions/26409316/how-do-i-extract-two-mutable-elements-from-a-vec-in-rust
            let copy_from;
            {
                let from = stages[pos - 1].clone() ;
                copy_from = from;
            }
            let to = &mut stages[pos];
            println!("finding best way ...");
            find_best_way(&copy_from,
                          to);
            println!("found best way");
        }
        println!("stage {} done", pos);
        pos = pos + 1;

        if (pos + 1) / 2 >= score.len() {
            // intermediate transition after tail note
            // is of no use
            break;
        }
        println!("creating state list for intermediate transition ...");
        let vec_finger_status: Vec<FingerStatus>
            = create_finger_status_list_playing_note(
            &note, true);
        let nodes: Vec<FingerStatusAndChoice> = vec_finger_status.iter().map( | f | FingerStatusAndChoice {
            finger_status: f.clone(),
            choice: Choice {prev_pos: -1, accumulate_cost: 0.0 }
        }).collect();
        stages.push(StageNodes{nodes});
        println!("created state list");

        let copy_from;
        {
            let from = stages.get(pos - 1).unwrap().clone();
            copy_from = from;
        }
        let to = stages.get_mut(pos).unwrap();


        println!("finding best way ...");
        find_best_way(&copy_from,
                          to);

        println!("found best way");
        println!("stage {} done", pos);
        pos = pos + 1;
    }
    println!("Computing done!");

    // find best solution backward
    let mut choice_pos_array = vec!(0; pos);
    for cur in (0..pos).rev() {
        let mut min_cost = 10000.0;
        let mut min_cost_pos: i32= -1;
        let stage = stages.get(cur).unwrap();
        if cur == pos - 1 {
            // look for final choice
            for i in 0..stage.nodes.len() {
                let cost = stage.nodes.get(i).unwrap().choice.accumulate_cost;
                if cost < min_cost {
                    min_cost_pos = i as i32;
                    min_cost = cost;
                }
            }
            choice_pos_array[cur] = min_cost_pos;
        } else {
            // look for other choice
            let stage_nodes: &StageNodes = stages.get(cur+1).unwrap();
            let back_pos = choice_pos_array[cur + 1];
            println!("back_pos = {}", back_pos);
            let node : &FingerStatusAndChoice = stage_nodes.nodes.get(back_pos as usize).unwrap();
            choice_pos_array[cur] = node.choice.prev_pos;
        }
    }

    let mut cur_stage : i32 = -1;
    let mut note_idx : i32 = -1;
    println!("stages.len = {}, pos = {}", stages.len(), pos);
    while cur_stage < pos as i32 {
        if cur_stage >= 1 {
            // transition
            let node : &FingerStatusAndChoice = stages.get(cur_stage as usize).unwrap().nodes.get(choice_pos_array[cur_stage as usize] as usize).unwrap();
            let node_from: &FingerStatusAndChoice = stages.get(cur_stage as usize).unwrap().nodes.get(choice_pos_array[cur_stage as usize -1] as usize).unwrap();
            let transition = compute_transition(&node_from.finger_status, &node.finger_status);
            // println!(" with transition: {:#?} to status {:#?}, and then ", transition, node.finger_status);
            println!("intermediate transition {:#?}", transition);
        }
        
        cur_stage += 1;
        note_idx += 1;
        
        let node : &FingerStatusAndChoice = stages.get(cur_stage as usize).unwrap().nodes.get(choice_pos_array[cur_stage as usize] as usize).unwrap();
        if cur_stage == 0 {
            // starting pose and first note
            println!("开始动作 {}\n 演奏{}",
                     finger_status_to_str(&node.finger_status), score.get( note_idx as usize).unwrap());
        } else {
            // second and etc notes
            let node_from: &FingerStatusAndChoice =
             stages.get(cur_stage as usize - 1).unwrap()
                .nodes.get(
                    choice_pos_array[cur_stage as usize -1]
                     as usize).unwrap();
            let transition = compute_transition(&node_from.finger_status, &node.finger_status);
            println!("\nand finally {:#?}, then play note {} = {:?}",
             transition, note_idx + 1, score.get(note_idx as usize).unwrap());
        }
        cur_stage += 1;
    }
    println!("end of score");
}
