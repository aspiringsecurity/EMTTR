import { motion } from "framer-motion";
export default function TransitionBoxes() {
  return;
}

export function FadeInWhenVisible({ children }) {
  return (
    <motion.div
      is="custom"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.5 }}
      variants={{
        visible: { opacity: 1 },
        hidden: { opacity: 0 },
      }}
    >
      {children}
    </motion.div>
  );
}

export function ComeFromTopWhenVisible({ children }) {
  return (
    <motion.div
      is="custom"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 1 }}
      variants={{
        visible: { y: 0, opacity: 1 },
        hidden: { y: -10, opacity: 0 },
      }}
    >
      {children}
    </motion.div>
  );
}

export function ComeFromBottomWhenVisible({ children }) {
  return (
    <motion.div
      is="custom"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 1 }}
      variants={{
        visible: { y: 0, opacity: 1 },
        hidden: { y: 10, opacity: 0 },
      }}
    >
      {children}
    </motion.div>
  );
}

export function ComeFromLeftWhenVisible({ children }) {
  return (
    <motion.div
      is="custom"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 1 }}
      variants={{
        visible: { x: 0, opacity: 1 },
        hidden: { x: -10, opacity: 0 },
      }}
    >
      {children}
    </motion.div>
  );
}

export function ComeFromRightWhenVisible({ children }) {
  return (
    <motion.div
      is="custom"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 1 }}
      variants={{
        visible: { x: 0, opacity: 1 },
        hidden: { x: 10, opacity: 0 },
      }}
    >
      {children}
    </motion.div>
  );
}
