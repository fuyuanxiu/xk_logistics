package com.system.user.dao;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.system.user.entity.SysUser;

public interface SysUserDao extends JpaRepository<SysUser, Long> {

    public int countByIsDelAndUserCode(Integer isDel, String userCode);

    public SysUser findByIsDelAndUserCode(Integer isDel, String userCode);
    
    public List<SysUser> findById(long id);

    public List<SysUser> findByUserIsSuperAndIdIn(Integer isSuper, List<Long> idList);

}
