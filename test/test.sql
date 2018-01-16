SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
set names utf8;

--
-- 数据库: `test_dev_1`
--
create database test_dev_1;
use test_dev_1;

-- --------------------------------------------------------
CREATE TABLE IF NOT EXISTS `t_log_login` (
  `role_id` bigint(20) unsigned NOT NULL COMMENT '角色ID',
  `account_name` varchar(50) NOT NULL COMMENT '平台账号',
  `role_level` int(11) NOT NULL COMMENT '等级',
  `user_ip` varchar(64) NOT NULL COMMENT 'IP',
  `os` varchar(32) NOT NULL COMMENT '操作系统',
  `os_version` varchar(32) NOT NULL COMMENT '操作系统版本号',
  `device` varchar(32) NOT NULL COMMENT '设备名称',
  `imei` varchar(100) NOT NULL COMMENT '设备唯一号',
  `screen` varchar(32) NOT NULL COMMENT '屏幕分辨率',
  `mno` varchar(32) NOT NULL COMMENT '运营商',
  `nm` varchar(32) NOT NULL COMMENT '移动网络接入方式',
  `platform` varchar(32) NOT NULL COMMENT '平台',
  `server_id` int(11) NOT NULL COMMENT '服务器ID',
  `m_time` int(11) NOT NULL COMMENT '日志记录时间',
  KEY `role_id` (`role_id`),
  KEY `m_time` (`m_time`),
  KEY `platform` (`platform`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='角色上线日志';

-- --------------------------------------------------------
CREATE TABLE IF NOT EXISTS `t_log_register` (
  `role_id` bigint(20) unsigned NOT NULL COMMENT '角色ID',
  `account_name` varchar(50) NOT NULL COMMENT '平台账号',
  `role_name` varchar(50) NOT NULL COMMENT '玩家名',
  `os` varchar(32) NOT NULL COMMENT '操作系统',
  `os_version` varchar(32) NOT NULL COMMENT '操作系统版本号',
  `device` varchar(32) NOT NULL COMMENT '设备名称',
  `imei` varchar(100) NOT NULL COMMENT '设备唯一号',
  `screen` varchar(32) NOT NULL COMMENT '屏幕分辨率',
  `mno` varchar(32) NOT NULL COMMENT '运营商',
  `nm` varchar(32) NOT NULL COMMENT '移动网络接入方式',
  `platform` varchar(32) NOT NULL COMMENT '平台',
  `server_id` int(11) NOT NULL COMMENT '服务器ID',
  `m_time` int(11) NOT NULL COMMENT '日志记录时间',
  KEY `role_id` (`role_id`),
  KEY `m_time` (`m_time`),
  KEY `platform` (`platform`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='角色创建日志';

INSERT INTO `t_log_register` (`role_id`, `account_name`, `role_name`, `os`, `os_version`, `device`, `imei`, `screen`, `mno`, `nm`, `platform`, `server_id`, `m_time`) VALUES
(100, 'randy', 'lovelovelove', 'iOS', '1.0', 'iphone 8', '9dc40db03f03a59f2735c9e495a9885a', '1280*720', '中国移动', '4G', '000011', 1, 1515588169),
(101, 'chen', 'djfksjfljg', 'Android', '5.6', 'xiaomi 4C', '8e21dfe112d94406d23d69666e4697f0', '1920*1080', '中国联通', '3G', '000011', 1, 1515588179);




--
-- 数据库: `test_dev_2`
--
create database test_dev_2;
use test_dev_2;

-- --------------------------------------------------------
CREATE TABLE IF NOT EXISTS `t_log_login` (
  `role_id` bigint(20) unsigned NOT NULL COMMENT '角色ID',
  `account_name` varchar(50) NOT NULL COMMENT '平台账号',
  `role_level` int(11) NOT NULL COMMENT '等级',
  `user_ip` varchar(64) NOT NULL COMMENT 'IP',
  `os` varchar(32) NOT NULL COMMENT '操作系统',
  `os_version` varchar(32) NOT NULL COMMENT '操作系统版本号',
  `device` varchar(32) NOT NULL COMMENT '设备名称',
  `imei` varchar(100) NOT NULL COMMENT '设备唯一号',
  `screen` varchar(32) NOT NULL COMMENT '屏幕分辨率',
  `mno` varchar(32) NOT NULL COMMENT '运营商',
  `nm` varchar(32) NOT NULL COMMENT '移动网络接入方式',
  `platform` varchar(32) NOT NULL COMMENT '平台',
  `server_id` int(11) NOT NULL COMMENT '服务器ID',
  `m_time` int(11) NOT NULL COMMENT '日志记录时间',
  KEY `role_id` (`role_id`),
  KEY `m_time` (`m_time`),
  KEY `platform` (`platform`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='角色上线日志';

-- --------------------------------------------------------
CREATE TABLE IF NOT EXISTS `t_log_register` (
  `role_id` bigint(20) unsigned NOT NULL COMMENT '角色ID',
  `account_name` varchar(50) NOT NULL COMMENT '平台账号',
  `role_name` varchar(50) NOT NULL COMMENT '玩家名',
  `os` varchar(32) NOT NULL COMMENT '操作系统',
  `os_version` varchar(32) NOT NULL COMMENT '操作系统版本号',
  `device` varchar(32) NOT NULL COMMENT '设备名称',
  `imei` varchar(100) NOT NULL COMMENT '设备唯一号',
  `screen` varchar(32) NOT NULL COMMENT '屏幕分辨率',
  `mno` varchar(32) NOT NULL COMMENT '运营商',
  `nm` varchar(32) NOT NULL COMMENT '移动网络接入方式',
  `platform` varchar(32) NOT NULL COMMENT '平台',
  `server_id` int(11) NOT NULL COMMENT '服务器ID',
  `m_time` int(11) NOT NULL COMMENT '日志记录时间',
  KEY `role_id` (`role_id`),
  KEY `m_time` (`m_time`),
  KEY `platform` (`platform`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='角色创建日志';
